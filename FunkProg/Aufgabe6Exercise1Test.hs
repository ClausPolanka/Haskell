module Aufgabe6Exercise1Test where

import HUnit
import Aufgabe6

surfaceCircle = 
    TestCase (assertEqual "Surface" 28.27433 (surface $ Circle 2 2 3))

surfaceCircle' = 
    TestCase 
    (assertEqual "Surface" 28.27433 (surface' $ Circle' (Point 2 2) 3))

createCar = 
    TestCase 
    (assertEqual "Car" 
      Car {company="Ford", model="Mustang", year=1967} 
      (Car {company="Ford", model="Mustang", year=1967}))

allTests = 
    TestList [
        TestLabel 
        " Given a circle with a radius of 3, calculate it's surface." 
        surfaceCircle,

        TestLabel 
        " Given a circle with a radius of 3, calculate it's surface." 
        surfaceCircle',

        TestLabel 
        " Create car with given data." 
        createCar
    ]

main = do runTestTT allTests