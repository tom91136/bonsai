package net.kurobako.bonsai.scalafix

import metaconfig.Conf

/** Reads rule sections of `.scalafix.conf` without derived decoders, which need Scala 2 macros. */
private object settings {
  def section(conf: Conf, name: String): Map[String, Conf] = conf match {
    case Conf.Obj(values) =>
      values.collectFirst { case (`name`, Conf.Obj(fields)) => fields.toMap }.getOrElse(Map.empty)
    case _ => Map.empty
  }

  def string(fields: Map[String, Conf], key: String): Option[String] = fields.get(key).collect { case Conf.Str(value) =>
    value
  }

  def bool(fields: Map[String, Conf], key: String): Option[Boolean] = fields.get(key).flatMap {
    case Conf.Bool(value)  => Some(value)
    case Conf.Str("true")  => Some(true)
    case Conf.Str("false") => Some(false)
    case _                 => None
  }

  def strings(fields: Map[String, Conf], key: String): List[String] = fields.get(key).toList.flatMap {
    case Conf.Lst(values) => values.collect { case Conf.Str(value) => value }
    case Conf.Str(value)  => List(value)
    case _                => Nil
  }

  def objects(fields: Map[String, Conf], key: String): List[Map[String, Conf]] = fields.get(key).toList.flatMap {
    case Conf.Lst(values) => values.collect { case Conf.Obj(entries) => entries.toMap }
    case _                => Nil
  }

  def table(fields: Map[String, Conf], key: String): Map[String, String] = fields.get(key).toList.flatMap {
    case Conf.Obj(entries) => entries.collect { case (name, Conf.Str(value)) => name -> value }
    case _                 => Nil
  }.toMap
}
