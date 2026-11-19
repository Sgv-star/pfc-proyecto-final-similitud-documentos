# Nota: fallo preexistente en el test de `fragmentos`

Esta nota no es uno de los cuatro informes del proyecto. Registra un problema
encontrado al ejecutar `./gradlew test` para que la siguiente persona sepa qué
pasó y no lo confunda con un error de las funciones nuevas.

## Síntoma

La prueba `fragmentos: conserva los fragmentos repetidos, uno por posición`
(`app/src/test/scala/proyecto/SimilitudTest.scala`, líneas 97-102) falla:

```scala
val ts = Vector("a", "b", "a", "b")
assert(fragmentos(ts, 2) ==
  Vector(Vector("a", "b"), Vector("b", "a"),
         Vector("a", "b"), Vector("b", "a")))
```

La función devuelve tres fragmentos, no cuatro.

## El error está en la prueba, no en la función

Con `t = 4` palabras y `k = 2`, hay exactamente `t - k + 1 = 3` ventanas
consecutivas:

1. posición 0: `(a, b)`
2. posición 1: `(b, a)`
3. posición 2: `(a, b)`

El cuarto `(b, a)` que espera la prueba empezaría en la posición 3, donde ya no
hay dos palabras disponibles; solo existiría si las ventanas fueran circulares,
lo cual no es lo que pide el enunciado.

La implementación actual de `fragmentos` devuelve correctamente esos tres
fragmentos y conserva el repetido `(a, b)` en su posición.

## Evidencia de que la prueba es ajena a la plantilla

- `git blame -L 97,102 -- app/src/test/scala/proyecto/SimilitudTest.scala`
  atribuye esas líneas al commit `142940a` («Añadidas las pruebas para la función
  fragmentos», 14 de noviembre de 2026), no al commit inicial de la plantilla
  `e3d51c3`.
- Existe en `origin/main`, así que el problema viene de antes de la rama
  `implementar-ranking-y-detectores`.
- La contradicen dos fuentes del propio repositorio:
  - `SimilitudTest.scala`, prueba «fragmentos: la cantidad es t - k + 1»
    (líneas 91-95): para `t = 6, k = 4` espera 3 fragmentos.
  - `MODELS.md`, §1.1.1: «Hay exactamente `t - k + 1` fragmentos cuando
    `t >= k`, y ninguno cuando `t < k»`, con `0 <= i <= t - k`.

## Corrección sugerida (NO aplicada)

Se dejó la prueba intacta porque el README indica que las pruebas existentes no
se modifican. Si se decide corregirla, hay dos alternativas que preservan su
intención de comprobar los fragmentos repetidos:

- Cambiar la entrada a `Vector("a", "b", "a", "b", "a")`: con `t = 5, k = 2`
  sí salen las cuatro ventanas que el aserto actual espera.
- Mantener la entrada de cuatro palabras y esperar
  `Vector(Vector("a", "b"), Vector("b", "a"), Vector("a", "b"))`.

## Estado al documentar

`./gradlew test`: 77 pruebas, 6 fallos. Cinco son las funciones paralelas que
todavía dicen `???` y el sexto es este test de `fragmentos`.
