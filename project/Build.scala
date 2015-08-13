/*
 * Copyright (c) 2011-15 Peter Schmitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._
import Keys._

// import com.typesafe.sbt.SbtGit._
// import GitKeys._

import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseCreateSrc

// import sbtbuildinfo._
// import sbtbuildinfo.BuildInfoKeys._

import spray.revolver.RevolverPlugin.Revolver

object GeomBuild extends Build {

  lazy val rtree = (project in file(".")).
  settings(Seq(
      organization := "de.petomat",
      version := "0.1",
      moduleName := "rtree",
      scalaVersion := "2.11.7",
      
      // (unmanagedSourceDirectories in Compile) <<= (scalaSource in Compile)(Seq(_)),
      // (unmanagedSourceDirectories in Test) <<= (scalaSource in Test)(Seq(_)),

      scalacOptions := Seq(
        "-feature",
//        "-language:higherKinds",
//        "-language:implicitConversions",
        "-Xfatal-warnings",
        "-deprecation",
        "-unchecked"),

      initialCommands in console := """import de.petomat.rtree._""",
      EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed,
      EclipseKeys.withSource := true,
      mainClass in Revolver.reStart := Some("de.petomat.rtree.Main"),
      libraryDependencies += "de.petomat" %% "geom" % "1.0"
    ) ++
    Revolver.settings ++
    addCommandAlias("refresh", ";reload;update;clean;compile;eclipse;version") ++
    addCommandAlias("cc", ";clean;~compile")
    )
    
}
