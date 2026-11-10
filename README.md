# bonsai

Linting and whole-program analysis for Scala 3. A compiler plugin checks code as it compiles, an analysis reads the TASTy and class files of the build, and Scalafix rules read its SemanticDB.

bonsai runs on sbt 1, sbt 2, and Maven, for Scala 3.3.4 and later. A Scala release without a matching compiler plugin builds without the compile checks.

## Usage

### Install

bonsai has no published release. Build it from GitHub:

```bash
git clone https://github.com/tom91136/bonsai.git
cd bonsai
sbt installLocal
```

The command publishes to `~/.ivy2/local` and, where Maven is installed, to `~/.m2`. Pull and rerun it to update. Each module replaces the one published in a single rename, so a build that resolves bonsai meanwhile never reads a half-written module. `-Dsbt.ivy.home` and `-Dmaven.repo.local` move the two repositories, and `MAVEN_COMMAND` names the Maven to use.

### sbt

Add this to `project/plugins.sbt`. A machine without bonsai, such as a CI runner, then loads the build without it:

```scala
if ((file(sys.props("user.home")) / ".ivy2/local/net.kurobako/sbt-bonsai_sbt2_3").isDirectory)
  Seq(addSbtPlugin("net.kurobako" % "sbt-bonsai" % "0.1.0-SNAPSHOT"))
else Nil
```

On sbt 1, test for `sbt-bonsai` in place of `sbt-bonsai_sbt2_3`.

Configure bonsai in `.bonsai.conf`. A `bonsai*` key in a build file stops the build loading where the plugin is absent.

`compile` runs the compile checks. The other two parts have their own commands:

```bash
sbt bonsaiAnalyse
sbt "scalafixAll --check"
```

### Maven

Add this profile to the root `pom.xml`, which already declares `scala-maven-plugin`. It activates only where bonsai is installed, so the reactor builds without it elsewhere:

```xml
<profile>
  <id>bonsai</id>
  <activation>
    <file>
      <exists>${user.home}/.m2/repository/net/kurobako/bonsai-maven-plugin</exists>
    </file>
  </activation>
  <build>
    <plugins>
      <plugin>
        <groupId>net.kurobako</groupId>
        <artifactId>bonsai-maven-plugin</artifactId>
        <version>0.1.0-SNAPSHOT</version>
        <executions>
          <execution>
            <id>prepare</id>
            <goals>
              <goal>prepare</goal>
            </goals>
          </execution>
          <execution>
            <id>check</id>
            <goals>
              <goal>analyse</goal>
              <goal>scalafix</goal>
            </goals>
          </execution>
        </executions>
      </plugin>
      <plugin>
        <groupId>net.alchim31.maven</groupId>
        <artifactId>scala-maven-plugin</artifactId>
        <configuration>
          <args combine.children="append">
            <arg>@${bonsai.scalac.args}</arg>
          </args>
        </configuration>
      </plugin>
    </plugins>
  </build>
</profile>
```

`mvn verify` runs all three parts. To check Java sources too, see `maven-plugin/src/it/java`.

### Modes

Set `BONSAI_MODE`:

- `relaxed`: the default. Findings are reported, and only error-level checks fail the build.
- `strict`: the default when `CI` is set. Compile findings fail the compile, so the analysis does not run after one.
- `audit`: compile findings warn, the analysis runs, and the run fails at the end with every finding in one report. Do not publish from this mode.

### Configuration

`.bonsai.conf` sits at the build root. Without one, or with an empty one, every rule is on. Each finding names its id as `[bonsai:<id>]`.

```hocon
disabled = ["unused-member"]        # off
nonfatal = ["same-type-arguments"]  # reports without failing
errors = ["acyclic"]                # fails in every mode
baseline = "bonsai-baseline.tsv"    # recorded analysis findings report without failing
excluded-modules = ["generated"]    # left out of the analysis
projects.api {
  library = true                    # other builds call its public code
}
```

Record the baseline with `sbt bonsaiBaselineWrite` or `BONSAI_WRITE_BASELINE=bonsai-baseline.tsv mvn verify`.

## Development

The build uses sbt 2. Run each line in its own sbt session, as `++` changes the Scala version of what the session publishes next:

```bash
sbt "scalafmtCheckAll; scalafmtSbtCheck; Test/compile; testFull; scalafixAll --check; analyse"
sbt "++3.3.9 plugin/testFull"
sbt scriptedAll
```

The Maven plugin tests resolve the tools from `~/.m2`:

```bash
sbt installLocal
(cd maven-plugin && mvn verify)
```

- `plugin`: compiler plugin, cross-built for each Scala 3 line.
- `analysis`: whole-program analysis.
- `scalafix`: Scalafix rules, built against the oldest Scalafix they run on.
- `java-plugin`: javac plugin for Java sources.
- `sbt-plugin`, `maven-plugin`: build integrations.
- `community`: the harness that runs bonsai over open-source projects, described in `COMMUNITY.md`.
