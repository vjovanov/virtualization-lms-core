package scala.virtualization.lms
package internal

trait Config {
  val verbosity = System.getProperty("lms.verbosity","2").toInt

}
