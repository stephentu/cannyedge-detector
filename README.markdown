Canny Edge Detector written in Scala
====================================
This is an implementation of the famous Canny edge detection algorithm written in Scala. In order for the code to work, the javax.imageio classes are required. There are no other dependencies.

Compiling
=========
Simply type `sbt compile` in the root project directory.

Usage
=====
Here is the simplest working example:

    import com.stephentu._

    import java.io._
    import javax.imageio._

    val edges = EdgeDetector.detectEdges(ImageIO.read(new File("myimage.png")))
