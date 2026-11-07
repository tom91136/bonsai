package net.kurobako.bonsai.analysis

object Root {

  /** `Main` enters the main methods of the type; `Exported` the public and protected members of a type other builds
    * use; `Reflective` every member of a type the build names in a string, which reflection reaches however private;
    * `Provided` the public constructor, the `provider` method and the overrides of a type a loader instantiates, as a
    * service, a compiler plugin or an sbt plugin is.
    */
  enum Kind {
    case Main, Exported, Reflective, Provided
  }
}

/** A type the program is entered through, and how. */
case class Root(name: String, kind: Root.Kind)
