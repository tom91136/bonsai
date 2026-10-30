package net.kurobako.baumkuchen.maven;

import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

import java.util.List;

/**
 * Collects the model of the Java code of the test sources of the module, which the javac plugin writes while the
 * compiler compiles them.
 *
 * <p>The model of the test code is written to a file of its own, so that the compile of the test code does not write
 * over the model of the main code.
 */
@Mojo(
  name = "java-test",
  defaultPhase = LifecyclePhase.PROCESS_TEST_SOURCES,
  threadSafe = false,
  requiresDependencyResolution = ResolutionScope.TEST
)
public class JavaTestMojo extends JavaModelMojo {

  @Override
  protected String scope() {
    return "test";
  }

  @Override
  protected List<String> roots() {
    return project.getTestCompileSourceRoots();
  }
}
