package net.kurobako.baumkuchen.maven;

import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

import java.util.List;

/**
 * Collects the model of the Java code of the main sources of the module, which the javac plugin writes while the
 * compiler compiles them.
 */
@Mojo(
  name = "java",
  defaultPhase = LifecyclePhase.PROCESS_SOURCES,
  threadSafe = false,
  requiresDependencyResolution = ResolutionScope.TEST
)
public class JavaMojo extends JavaModelMojo {

  @Override
  protected String scope() {
    return "main";
  }

  @Override
  protected List<String> roots() {
    return project.getCompileSourceRoots();
  }
}
