Trabajo sobre la asignatura Programación Declarativa

Este trabajo consistirá en la optimización de funciones mediante algoritmo multiobjetivo basado en agregación.

Para compilar el programa, solo tenemos que ejecutar el siguiente comando:
    ghc -o Principal.hs principal.exe

Si nos diese el siguiente error, 
    Could not load module ‘System.Random’
simplemente tendremos que ejecutar
    ghc -o Principal.hs principal.exe -package random
