
package desir

import gov.nasa.jpf._
import java.io._

object Main {
	def getListOfFiles(dir: String) : List[File] = {
	    val d = new File(dir)
	    if (d.exists && d.isDirectory) {
	        d.listFiles.filter(_.isFile).toList
	    } else {
	        List[File]()
    	}
	}

	def main(args: Array[String]): Unit = {
		val config = new Config(Array[String]()) //new File("test.jpf").getAbsolutePath)
		val alljars = getListOfFiles(".").filter(_.getName.endsWith(".jar")).map(_.getName).mkString(",")
		config.setProperty("native_classpath", "${jpf-core}/build/jpf.jar,.," + alljars)
		config.setProperty("classpath", ".," + alljars)
		config.setTarget("Main")

		val jpf = new JPF(config)

		val target = args(0)
		val desirListener = new desir.jpf.DesIRListener(target)
		jpf.addVMListener(desirListener)
		jpf.run

		CollectingSemantics.execute(
			desirListener.frame, 
			desirListener.analyzer, 
			desirListener.execution)

		System.exit(0)

		// val trace = desirListener.execution.foldLeft(Trace.empty(desirListener.analyzer.locals)) {
		// 	case (trace, inst) => 
		// 		inst.transform(trace)
		// }.exec

		// trace.foreach { println }
	}
}

// List(0, 1, 2, 3, 4, 5, 16, 17, 18, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38)
// List(0, 1, 2, 3, 4, 5, 8, 11, 12, 15)