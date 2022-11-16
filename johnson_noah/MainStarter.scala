package johnson_noah

import java.io.{FileNotFoundException, FileWriter}
import java.text.DecimalFormat
import scala.io.StdIn
import scala.xml.{PrettyPrinter, XML}
import johnson_noah.Ingredient
import johnson_noah.RecipeBook
import johnson_noah.Baked
import johnson_noah.Mix
import johnson_noah.Single
import johnson_noah.Remeasure


object MainStarter {

  def main(args: Array[String]) = {
    val recipeBook = new RecipeBook
    val format = new DecimalFormat("0.#")
    val menu: String =
      """
        |1) Add Data
        |2) Display Data
        |3) Remove Recipe
        |4) Load XML
        |5) Write XML
        |6) Find Ingredient in Recipe
        |7) Calculate Calories
        |8) Calculate Volume
        |9) Calculate Calorie Density
        |0) Quit
        |
        |Choice:> """.stripMargin
    var choice: Any = -1
    var temp = ""

    while (choice != "0") {
      print("\n" + menu)

      //something to strip out empty lines
      temp = StdIn.readLine()
      while (temp.isEmpty)
        temp = StdIn.readLine()

      choice = temp


      if (choice == "1") {
        print("What recipe:> ")
        val newRecipeName = StdIn.readLine()
        if (!recipeBook.recipeExists(newRecipeName)) {
          val newRecipe = new Recipe(newRecipeName)
          // GRADING: ADD
          val newIngredient = createIngredient()
          newRecipe.addIngredient(newIngredient)
          recipeBook.addRecipe(newRecipe)
        }
        else {
          print(newRecipeName + " is already in the book")
        }
      }


      else if (choice == "2") {
        // GRADING: PRINT
        print(recipeBook.getString)
      }


      else if (choice == "3") {
        println("What recipe:> ")
        val recipeToRemove = StdIn.readLine()
        val removeSuccess: Boolean = recipeBook.removeRecipe(recipeToRemove)
        if (removeSuccess)
          println("Removed " + recipeToRemove)
        else
          println("Recipe not found")
      }

      else if (choice == "4") {
        print("file name:> ")
        val fileName = StdIn.readLine()
        try {
          var topNode = XML.loadFile(fileName)
          if (topNode.label != "recipebook") {
            println("invalid xml file. needs to be an recipebook xml file")
          }
          else {
            // GRADING: READ
            recipeBook.loadXML(topNode)
          }
        }
        catch {
          case e: FileNotFoundException => {
            println("Could not open file: " + e.getMessage)
          }
        }
      }

      else if (choice == "5") {
        print("File name:> ")
        val fileName = StdIn.readLine()
        // GRADING: WRITE
        var xmlTree = recipeBook.writeXML()
        val prettyPrinter = new scala.xml.PrettyPrinter(80, 3)
        val prettyXml = prettyPrinter.format(xmlTree)
        val write = new FileWriter(fileName)
        write.write(prettyXml)
        write.close()
      }

      else if (choice == "6") {
        print("recipe:> ")
        val ingredientToFind = StdIn.readLine()
        // GRADING: FIND
        val foundRecipe = recipeBook.findRecipe(ingredientToFind).orNull
        if(foundRecipe == null) {
          print(ingredientToFind + " not found")
        }
        else {
          print(ingredientToFind + " found in " + foundRecipe.getName)
        }
      }

      else if (choice == "7") {
        print("What recipe:> ")
        val recipeToCalculate = StdIn.readLine()
        println("Calorie Count: " + format.format(recipeBook.calcCalories(recipeToCalculate)))
      }

      else if (choice == "8") {
        print("What recipe:> ")
        val recipeToCalculate = StdIn.readLine()
        println("Volume in Cups: " + format.format(recipeBook.calcVolume(recipeToCalculate)))
      }

      else if (choice == "9") {
        recipeBook.calcCalorieDensity()
      }
    }
  }


  def createIngredient(i: String = ""): Ingredient = {
    var ingredientType = ""
    if (i == "") {
      print("What ingredient (mix, baked, remeasure, single):> ")
      var newIngredientType = StdIn.readLine().toLowerCase()
      if (newIngredientType == "mix" || newIngredientType == "m") {
        ingredientType = "Mix"
      }
      if (newIngredientType == "single" || newIngredientType == "s") {
        ingredientType = "Single"
      }
      if (newIngredientType == "baked" || newIngredientType == "b") {
        ingredientType = "Baked"
      }
      if (newIngredientType == "remeasure" || newIngredientType == "r") {
        ingredientType = "Remeasure"
      }
    }
    else {
      ingredientType = i
    }
    if (ingredientType == "Single") {

      print("Name:> ")
      val singleIngredientName = StdIn.readLine()

      print("Calories:> ")
      val numCaloriesInput = StdIn.readLine()
      val numCalories = numCaloriesInput.toDouble.round.toInt

      print("Cups:> ")
      var numCups = StdIn.readLine().toDouble
      val newSingleIngredient = new Single(singleIngredientName, numCalories, numCups)

      print("\nAdded single")
      newSingleIngredient
    }

    else if (ingredientType == "Remeasure") {

      print("New Quantity:> ")
      val newQuantityInput = StdIn.readLine()
      val newQuantity = newQuantityInput.toDouble

      val remeasureIngredient = createIngredient()
      val remeasure = new Remeasure(newQuantity, remeasureIngredient)

      print("\nAdded remeasure")
      remeasure
    }

    else if (ingredientType == "Baked") {

      print("Name:> ")
      val bakedIngredientName = StdIn.readLine()

      print("Expansion Factor:> ")
      val expansionFactorInput = StdIn.readLine()
      val expansionFactor = expansionFactorInput.toDouble

      val bakedChild = createIngredient()
      val newBakedIngredient = new Baked(bakedIngredientName, expansionFactor, bakedChild)

      print("\nAdded baked")
      newBakedIngredient
    }

    else {
      print("Name:> ")
      val mixIngredientName = StdIn.readLine()
      val newMixIngredient = new Mix(mixIngredientName)

      var continue = "y"
      while (continue == "y") {
        val newIngredient = createIngredient()
        newMixIngredient.addIngredient(newIngredient)
        print("\nAdd another ingredient (y/n):> ")
        continue = StdIn.readLine().toLowerCase
      }

      print("\nAdded mix")
      newMixIngredient
    }
  }
}