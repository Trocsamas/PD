Trabajo sobre la asignatura Programación Declarativa

Este trabajo consistirá en la optimización de funciones mediante algoritmo multiobjetivo basado en agregación.

Las librerias usadas para el desarrollo del programa han sido las siguientes:
    - System.Directory
    - System.Random
    - System.IO
    - Data.Array
    - Data.Time
    - Data.List
    - Data.Char
    - Graphics.Gnuplot, para hacer uso de ella hemos tenido que instalarla mediante cabal
        cabal install gnuplot

Para compilar el programa, solo tenemos que ejecutar el siguiente comando:
    ghc -o Principal.hs principal.exe

Si nos diese el siguiente error, 
    Could not load module ‘System.Random’
simplemente tendremos que ejecutar
    ghc -o Principal.hs principal.exe -package random
