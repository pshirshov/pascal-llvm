package pascal

import java.nio.file.{Files, Paths}
import scala.util.{Try, Success, Failure}
import scala.sys.process.*

object Main:

  def main(args: Array[String]): Unit =
    if args.length < 2 then
      Console.err.println("Usage: pascal-compiler <input.pas> <output>")
      System.exit(1)

    val inputPath = args(0)
    val outputPath = args(1)

    try
      // Read source file
      val source = Files.readString(Paths.get(inputPath))

      // Parse
      val program = Parser.parseProgram(source) match
        case Right(prog) => prog
        case Left(error) =>
          Console.err.println(s"Parse error: $error")
          System.exit(1)
          throw new RuntimeException() // unreachable

      // Type check
      try
        TypeChecker.checkProgram(program)
      catch
        case e: TypeError =>
          Console.err.println(s"Type error: ${e.getMessage}")
          System.exit(1)

      // Generate code
      try
        CodeGen.compileToObjectFile(program, s"$outputPath.o")

        // Link with gcc
        val exitCode = Seq("gcc", s"$outputPath.o", "-o", outputPath).!
        if exitCode != 0 then
          Console.err.println("Linking failed")
          System.exit(1)

        // Clean up object file
        Files.deleteIfExists(Paths.get(s"$outputPath.o"))

        println(s"Successfully compiled $inputPath to $outputPath")

      catch
        case e: CodegenError =>
          Console.err.println(s"Code generation error: ${e.getMessage}")
          System.exit(1)

    catch
      case e: Exception =>
        Console.err.println(s"Error: ${e.getMessage}")
        e.printStackTrace()
        System.exit(1)
