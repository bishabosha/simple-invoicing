object Logger:
  def info(msg: String) =
    Console.err.println(msg.linesWithSeparators.map(l => s"[INFO] $l").mkString)
  def error(msg: String) =
    Console.err.println(
      msg.linesWithSeparators.map(l => s"[Error] $l").mkString
    )
