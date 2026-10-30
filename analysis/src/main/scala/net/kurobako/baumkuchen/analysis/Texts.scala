package net.kurobako.baumkuchen.analysis

import java.nio.charset.StandardCharsets
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.PathMatcher
import scala.util.Try
import scala.util.matching.Regex

/** Rules over the text of every file git sees in the build, whatever its language: terms and patterns a file may not
  * contain, binary files, licence notices in source, and lines that need a blank line before them or after their block.
  * The notices of licences, which name the terms of contracts, are not searched for terms. Globs match the path from
  * the build root with a leading slash, so a glob that starts with `**` and a slash matches files at the root too. A
  * line containing `baumkuchen:ignore <rule>` exempts itself and the line after it from that rule.
  */
object Texts {

  enum Kind(val id: String, val rule: Finding.Rule) {
    case Terms    extends Kind("terms", Finding.Rule.ForbiddenTerm)
    case Pattern  extends Kind("patterns", Finding.Rule.ForbiddenPattern)
    case Binary   extends Kind("binary", Finding.Rule.BinaryFile)
    case Licence  extends Kind("licences", Finding.Rule.LicenceNotice)
    case Preceded extends Kind("preceded", Finding.Rule.BlankLineBefore)
    case Followed extends Kind("followed", Finding.Rule.BlankLineAfter)
  }

  /** A configured rule: `pattern` is what a file may not contain, or the lines that need a blank line before them or
    * after their block, which goes on through lines matching it, `continues` or `neutral`.
    */
  case class Rule(
      kind: Kind,
      level: Finding.Level,
      files: Vector[PathMatcher],
      exclude: Vector[PathMatcher],
      words: Vector[String],
      pattern: Option[Regex],
      continues: Option[Regex],
      neutral: Option[Regex],
      message: Option[String]
  )

  // Git's own test for a binary file.
  private val Probe  = 8000
  private val Ignore = """baumkuchen:ignore ([a-z*-]+)""".r

  private val Extensions =
    Vector(
      "scala",
      "java",
      "kt",
      "c",
      "cc",
      "cpp",
      "cxx",
      "h",
      "hh",
      "hpp",
      "hxx",
      "cu",
      "cuh",
      "hip",
      "inc",
      "inl",
      "py",
      "sh",
      "rs",
      "go",
      "js",
      "ts"
    )
  private val SourceFiles = Vector(s"**.{${Extensions.mkString(",")}}")

  private val Comments = Vector("//", "#", "*")

  // Phrases only the text of a licence contains, whichever licence it is.
  private val Bodies = Vector(
    "Permission is hereby granted, free of charge",
    "Redistribution and use in source and binary forms",
    "you may not use this file except in compliance with the License",
    "THE SOFTWARE IS PROVIDED \"AS IS\"",
    "This program is free software; you can redistribute it",
    "This Source Code Form is subject to the terms of the Mozilla Public"
  )

  // A notice may also only name its licence, which is how a source refers to the text it keeps elsewhere.
  private val Notices = Bodies ++
    Vector("Licensed under the Apache License", "Licensed to the Apache Software Foundation")

  /** The rules `fields` describe, by the index the manifest gives each, or what is wrong with them. */
  def rules(fields: Map[Int, Map[String, Vector[String]]]): Either[String, Vector[Rule]] =
    fields.toVector.sortBy(_._1).foldLeft[Either[String, Vector[Rule]]](Right(Vector.empty)) {
      case (rules, (index, rule)) => rules.flatMap(found => parse(index, rule).map(found :+ _))
    }

  private def parse(index: Int, fields: Map[String, Vector[String]]): Either[String, Rule] = {
    def one(field: String)   = fields.get(field).flatMap(_.lastOption)
    def regex(field: String) = one(field) match {
      case Some(text) =>
        Try(text.r).toEither.left.map(error => s"text rule $index: $field: ${error.getMessage}").map(Some(_))
      case None => Right(None)
    }
    for {
      kind  <- one("kind").flatMap(id => Kind.values.find(_.id == id)).toRight(s"text rule $index: unknown kind")
      level <- one("level").getOrElse("error") match {
        case "error"   => Right(Finding.Level.Error)
        case "warning" => Right(Finding.Level.Warning)
        case "info"    => Right(Finding.Level.Info)
        case other     => Left(s"text rule $index: level must be error, warning or info, not $other")
      }
      pattern   <- regex("pattern")
      continues <- regex("continues")
      neutral   <- regex("neutral")
      words = fields.getOrElse("words", Vector.empty)
      _ <- kind match {
        case Kind.Terms if words.isEmpty             => Left(s"text rule $index: terms take words")
        case Kind.Terms | Kind.Binary | Kind.Licence => Right(())
        case _ if pattern.isEmpty                    => Left(s"text rule $index: ${kind.id} take a pattern")
        case _ if one("message").isEmpty             => Left(s"text rule $index: ${kind.id} take a message")
        case _                                       => Right(())
      }
    } yield Rule(
      kind,
      level,
      matchers(fields.getOrElse("files", if (kind == Kind.Licence) SourceFiles else Vector("**"))),
      matchers(fields.getOrElse("exclude", Vector.empty)),
      words,
      pattern,
      continues,
      neutral,
      one("message")
    )
  }

  def matchers(globs: Vector[String]): Vector[PathMatcher] =
    globs.map(glob => FileSystems.getDefault.getPathMatcher(s"glob:$glob"))

  /** The findings of `rules` over the files git sees under `root`, leaving out those `excluded` matches, with the files
    * git could not list as a problem.
    */
  def findings(root: Path, excluded: Vector[PathMatcher], rules: Vector[Rule]): Either[String, Vector[Finding]] =
    if (rules.isEmpty) Right(Vector.empty)
    else
      listed(root).map(_.filter(path => !excluded.exists(_.matches(rooted(path)))).sorted.flatMap { path =>
        val applied = rules.filter(rule =>
          rule.files.exists(_.matches(rooted(path))) && !rule.exclude.exists(_.matches(rooted(path)))
        )
        if (applied.isEmpty) Vector.empty else check(path, Files.readAllBytes(root.resolve(path)), applied)
      })

  /** Source files the text rules exclude by name, which a rule's findings in them are better fixed than hidden by. */
  def excludedSources(root: Path, globs: Vector[String]): Vector[Finding] =
    globs.distinct.sorted.collect {
      case glob
          if glob.startsWith("/") && !glob.exists("*?[]{}\\".contains(_)) &&
            Extensions.exists(extension => glob.endsWith(s".$extension")) &&
            Files.isRegularFile(root.resolve(glob.drop(1))) =>
        Finding(
          Finding.Rule.ExcludedSource,
          node(glob.drop(1), 1),
          s"$glob is excluded from the text rules by name; fix its findings or exempt their lines with baumkuchen:ignore"
        )
    }

  private def rooted(path: String): Path = Path.of(s"/$path")

  private def listed(root: Path): Either[String, Vector[String]] =
    Try {
      val process = ProcessBuilder("git", "ls-files", "-z", "--cached", "--others", "--exclude-standard")
        .directory(root.toFile).redirectErrorStream(true).start()
      val output = String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      // The output is read to its end, so the process has finished or is about to.
      if (process.waitFor(1, java.util.concurrent.TimeUnit.MINUTES)) (process.exitValue(), output)
      else {
        process.destroy()
        (-1, "timed out")
      }
    }.toEither.left.map(_.getMessage).flatMap {
      case (0, output) =>
        Right(output.split('\u0000').toVector.filter(_.nonEmpty).distinct.filter(path =>
          Files.isRegularFile(root.resolve(path))
        ))
      case (_, output) => Left(s"git ls-files failed in $root: ${output.trim}")
    }

  /** The findings of `rules` in one file's `bytes`. */
  def check(path: String, bytes: Array[Byte], rules: Vector[Rule]): Vector[Finding] = {
    val binary       = bytes.view.take(Probe).contains(0: Byte)
    lazy val lines   = String(bytes, StandardCharsets.UTF_8).split("\n", -1).toVector.map(_.stripSuffix("\r"))
    lazy val ignored = lines.zipWithIndex.flatMap((line, index) =>
      Ignore.findAllMatchIn(line).flatMap(found => Vector((index, found.group(1)), (index + 1, found.group(1))))
    ).toSet
    def exempt(index: Int, rule: Rule) = ignored((index, rule.kind.rule.id)) || ignored((index, "*"))
    rules.flatMap { rule =>
      val found = rule.kind match {
        case Kind.Binary =>
          Option.when(binary)(
            0 -> rule.message.getOrElse("is binary; embed it as text with the commands that produce it")
          ).toVector
        case _ if binary  => Vector.empty
        case Kind.Licence =>
          licensed(lines, Bodies).headOption.map(notice =>
            notice.start -> rule.message.getOrElse(
              "holds a licence notice; keep the licence text in a LICENSE resource and refer to it"
            )
          ).toVector
        case Kind.Terms =>
          val terms  = rule.words.map(word => word -> term(word))
          val notice = licensed(lines, Notices)
          lines.zipWithIndex.flatMap((line, index) =>
            terms.collect {
              case (word, regex) if regex.findFirstIn(line).isDefined && !notice.exists(_.contains(index)) =>
                index -> rule.message.fold(s"$word is a forbidden term")(message => s"$word: $message")
            }
          )
        case Kind.Pattern =>
          lines.zipWithIndex.collect {
            case (line, index) if rule.pattern.exists(_.findFirstIn(line).isDefined) => index -> rule.message.get
          }
        case Kind.Preceded =>
          lines.zipWithIndex.collect {
            case (line, index) if index > 0 && matching(rule.pattern, line) && lines(index - 1).trim.nonEmpty =>
              index -> rule.message.get
          }
        case Kind.Followed => followed(lines, rule).map(_ -> rule.message.get)
      }
      found.collect {
        case (index, message) if !exempt(index, rule) =>
          Finding(rule.kind.rule, node(path, index + 1), message, rule.level)
      }
    }
  }

  /** The lines of each licence notice: from the line holding its first phrase to the end of the comment around it. */
  private def licensed(lines: Vector[String], phrases: Vector[String]): Vector[Range] =
    lines.indices.filter(index => phrases.exists(lines(index).contains)).foldLeft(Vector.empty[Range]) { (found, start) =>
      if (found.exists(_.contains(start))) found else found :+ (start to commentEnd(lines, start))
    }

  // A block comment runs to its closing line, and a run of line comments to its last line.
  private def commentEnd(lines: Vector[String], start: Int): Int = {
    val opened = lines.lastIndexWhere(_.contains("/*"), start)
    if (opened >= 0 && !lines.slice(opened, start).exists(_.contains("*/")))
      lines.indices.drop(start).find(lines(_).contains("*/")).getOrElse(lines.size - 1)
    else {
      def comment(line: String) = Comments.exists(line.trim.startsWith)
      if (!comment(lines(start))) start
      else lines.indices.drop(start).takeWhile(index => comment(lines(index))).lastOption.getOrElse(start)
    }
  }

  private def matching(pattern: Option[Regex], line: String): Boolean = pattern.exists(_.findFirstIn(line).isDefined)

  /** The lines that end a block without a blank line: the first line after it that none of its patterns matches. */
  private def followed(lines: Vector[String], rule: Rule): Vector[Int] =
    lines.indices.foldLeft((Vector.empty[Int], false)) { case ((found, inside), index) =>
      val line = lines(index)
      if (matching(rule.pattern, line)) (found, true)
      else if (inside && (matching(rule.continues, line) || matching(rule.neutral, line))) (found, true)
      else if (inside && line.trim.nonEmpty) (found :+ index, false)
      else (found, false)
    }._1

  /** The word, its plural, and its capitalised and upper-case forms inside identifiers. */
  private def term(word: String): Regex = {
    val quoted      = Regex.quote(word)
    val capitalised = Regex.quote(word.capitalize)
    val upper       = Regex.quote(word.toUpperCase(java.util.Locale.ROOT))
    s"\\b${quoted}s?\\b|${capitalised}s?(?=\\z|[^a-z])|${upper}S?(?=\\z|[^A-Z])".r
  }

  private def node(path: String, line: Int): Node =
    Node(path, "", Output.Scope.Main, path, line, generated = false, exported = false, outer = None)
}
