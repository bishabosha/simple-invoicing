//> using scala 3.8.3
//> using platform native
//> using nativeVersion 0.5.11
//> using toolkit 0.9.2
//> using dep io.github.bishabosha::scala-object-notation::0.2.2-RC1
//> using files ${.}/../Configs.scala ${.}/../Logger.scala

val configPath =
  args.lift(0).getOrElse(throw new IllegalArgumentException("No config file provided"))

val _ = configs.readConfig(os.Path(configPath, os.pwd))
Logger.info(s"Config loaded successfully from $configPath")
