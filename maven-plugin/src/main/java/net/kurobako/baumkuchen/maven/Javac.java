package net.kurobako.baumkuchen.maven;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

/**
 * The javac plugin of the build, which collects the model of the Java code the compiler compiles, which the analysis
 * reads.
 *
 * <p>The plugin is a compiler plugin, so the compiler finds it on its processor path, and it writes the model to the
 * file the argument of the plugin names. The goal that runs before the compile sets the path of that file in the
 * {@code baumkuchen.model} property of the project, and the build names the processor path in the
 * {@code baumkuchen.java.plugin} property, which the compiler plugin of the build reads:
 *
 * <pre>{@code
 * <plugin>
 *   <groupId>org.apache.maven.plugins</groupId>
 *   <artifactId>maven-compiler-plugin</artifactId>
 *   <configuration>
 *     <compilerArgs>
 *       <arg>-processorpath</arg>
 *       <arg>${baumkuchen.java.plugin}</arg>
 *       <arg>-Xplugin:baumkuchen ${baumkuchen.model}</arg>
 *     </compilerArgs>
 *   </configuration>
 * </plugin>
 * }</pre>
 *
 * <p>The model of the main code and the model of the test code are written to files of their own, so that the two
 * compiles of a module do not write over each other.
 */
public final class Javac {

  /** The property the path of the file the model is written to is named by, which the compiler passes to the plugin. */
  public static final String MODEL = "baumkuchen.model";

  /** The property the processor path of the compiler is named by, which the build's compiler plugin reads. */
  public static final String PLUGIN = "baumkuchen.java.plugin";

  /** The artifact the javac plugin is published as, which the build resolves. */
  public static final String ARTIFACT = "baumkuchen-java-plugin_3";

  private Javac() {
  }

  /** The file the model of a scope is written to, which the analysis reads. */
  public static Path model(Path work, String scope) {
    return work.resolve("model").resolve(scope + ".tsv");
  }

  /** The processor path of the compiler, which holds the plugin and the library it runs on.
    *
    * <p>The path holds the jars the compiler reads, separated by the separator of the platform, as the compiler takes
    * it.
    */
  public static String processorPath(List<File> files) {
    return files.stream()
      .map(File::getAbsolutePath)
      .reduce((first, second) -> first + File.pathSeparator + second)
      .orElse("");
  }
}
