package org.thinkmeta.mathengine

import java.util.Properties

/**
 * 
 * @author hkarim
 * @since 7/15/11
 *
 */

object MathEngine {

  val config = {
    val props = new Properties
    props.load(
      Thread.currentThread.getContextClassLoader.getResourceAsStream(
       "org/thinkmeta/mathengine/engine-config.properties"
      ))
    props
  }

}