import Test.HUnit
import TP1

-- TESTS

testsInvertido :: Test
testsInvertido = TestList
  [ "Caja invertida (1)"
    ~: invertido cajaOn
    ~?= cajaOn
  , "Caja invertida (2)"
    ~: invertido cajaOff
    ~?= cajaOff
  , "Caja invertida (3)"
    ~: invertido cajaNada
    ~?= cajaNada
  , "Serie invertida cambia el orden de sus dos partes"
    ~: invertido (Serie cajaOn cajaOff)
    ~?= Serie cajaOff cajaOn
  , "Serie anidada invertida invierte recursivamente y cambia el orden"
    ~: invertido (Serie (Serie cajaOn cajaOff) cajaNada)
    ~?= Serie cajaNada (Serie cajaOff cajaOn)
  , "Paralelo invertido intercambia entrada/salida y ramales"
    ~: invertido (Paralelo on cajaOn cajaOff off)
    ~?= Paralelo off cajaOff cajaOn on
  , "Paralelo invertido invierte recursivamente los ramales"
    ~: invertido (Paralelo on (Serie cajaOn cajaOff) cajaNada off)
    ~?= Paralelo off cajaNada (Serie cajaOff cajaOn) on
  , "Invertir dos veces devuelve el circuito original"
    ~: invertido (invertido (Paralelo on (Serie cajaOn cajaOff) cajaNada off))
    ~?= Paralelo on (Serie cajaOn cajaOff) cajaNada off
  ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList
  [ "Caja encendida tiene camino iluminado"
    ~: hayCaminoIluminado cajaOn
    ~?= True
  , "Caja apagada no tiene camino iluminado"
    ~: hayCaminoIluminado cajaOff
    ~?= False
  , "Caja Nada no tiene camino iluminado"
    ~: hayCaminoIluminado cajaNada
    ~?= False
  , "Serie de dos encendidas tiene camino iluminado"
    ~: hayCaminoIluminado (Serie cajaOn cajaOn)
    ~?= True
  , "Serie donde alguna esta apagada no tiene camino iluminado"
    ~: hayCaminoIluminado (Serie cajaOn cajaOff)
    ~?= False
  , "Paralelo con entrada/salida encendidas y al menos un ramal iluminado"
    ~: hayCaminoIluminado (Paralelo on cajaOn cajaOff on)
    ~?= True
  , "Paralelo con entrada apagada no tiene camino iluminado"
    ~: hayCaminoIluminado (Paralelo off cajaOn cajaOn on)
    ~?= False
  , "Paralelo con salida apagada no tiene camino iluminado"
    ~: hayCaminoIluminado (Paralelo on cajaOn cajaOn off)
    ~?= False
  , "Paralelo con ambos ramales apagados no tiene camino iluminado"
    ~: hayCaminoIluminado (Paralelo on cajaOff cajaOff on)
    ~?= False
  ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
  , "Cantidad prendidas en caja apagada es 0"
    ~: cantidadPrendidas cajaOff
    ~?= 0
  , "Cantidad prendidas en caja Nada es 0"
    ~: cantidadPrendidas cajaNada
    ~?= 0
  , "Cantidad prendidas en serie de dos encendidas es 2"
    ~: cantidadPrendidas (Serie cajaOn cajaOn)
    ~?= 2
  , "Cantidad prendidas en serie con una apagada es 1"
    ~: cantidadPrendidas (Serie cajaOn cajaOff)
    ~?= 1
  , "Cantidad prendidas en paralelo cuenta entrada, ramales y salida"
    ~: cantidadPrendidas (Paralelo on cajaOn cajaOff on)
    ~?= 3
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList
  [ "La lista de cajas de un circuito con una única caja encendida es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on]
  , "La lista de cajas de un circuito con una única caja apagada es la lista con esa caja"
    ~: cajasDeCircuito cajaOff
    ~?= [off]
  , "La lista de cajas de un circuito con una única caja vacía es la lista con esa caja"
    ~: cajasDeCircuito cajaNada
    ~?= [Nada]
  , "En una serie, las cajas aparecen en orden: primero las del circuito inicial, luego las del final"
    ~: cajasDeCircuito (Serie cajaOn cajaOff)
    ~?= [on, off]
  , "En un paralelo, las cajas aparecen en orden: entrada, izquierda, derecha, salida"
    ~: cajasDeCircuito (Paralelo on cajaOn cajaOff off)
    ~?= [on, on, off, off]
  , "Circuito de ejemplo del enunciado"
    ~: cajasDeCircuito
         ( Serie
             ( Paralelo
                 on
                 (Paralelo off cajaNada cajaOn on)
                 (Paralelo Nada cajaOn cajaOff Nada)
                 on
             )
             cajaOn
         )
    ~?= [on, off, Nada, on, on, Nada, on, off, Nada, on, on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
  , "Una serie que asocia a la izquierda es prolija (ejemplo del enunciado)"
    ~: esCircuitoProlijo (Serie (Serie cajaOn cajaOff) cajaOn)
    ~?= True
  , "Una serie que asocia a la derecha no es prolija (ejemplo del enunciado)"
    ~: esCircuitoProlijo (Serie cajaOn (Serie cajaOff cajaOn))
    ~?= False
  , "Un paralelo con ambos subcircuitos prolijos es prolijo"
    ~: esCircuitoProlijo (Paralelo on cajaOn cajaOff off)
    ~?= True
  , "Un paralelo con un subcircuito no prolijo no es prolijo"
    ~: esCircuitoProlijo (Paralelo on (Serie cajaOn (Serie cajaOff cajaOn)) cajaOff off)
    ~?= False
  ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
  -- "show = showDeCircuitoConEstructura".
  -- De esa forma, podrán distinguir la estructura de los circuitos en serie.
testsCircuitoEmprolijado :: Test
testsCircuitoEmprolijado = TestList
  [ "La versión emprolijada de una caja es la misma caja"
    ~: circuitoEmprolijado cajaOn
    ~?= cajaOn
  , "Un circuito ya prolijo queda igual"
    ~: circuitoEmprolijado (Serie (Serie cajaOn cajaOff) cajaOn)
    ~?= Serie (Serie cajaOn cajaOff) cajaOn
  , "Un circuito no prolijo se corrige asociando a la izquierda"
    ~: circuitoEmprolijado (Serie cajaOn (Serie cajaOff cajaOn))
    ~?= Serie (Serie cajaOn cajaOff) cajaOn
  , "Una serie con series en ambas partes se emprolija por completo"
    ~: circuitoEmprolijado (Serie (Serie cajaOn cajaNada) (Serie cajaOff cajaOn))
    ~?= Serie (Serie (Serie cajaOn cajaNada) cajaOff) cajaOn
  , "Un paralelo con subcircuitos ya prolijos queda igual"
    ~: circuitoEmprolijado (Paralelo on (Serie cajaOn cajaOff) cajaOn off)
    ~?= Paralelo on (Serie cajaOn cajaOff) cajaOn off
  , "Un paralelo con un subcircuito no prolijo se emprolija por dentro"
    ~: circuitoEmprolijado (Paralelo on (Serie cajaOn (Serie cajaOff cajaOn)) cajaOff off)
    ~?= Paralelo on (Serie (Serie cajaOn cajaOff) cajaOn) cajaOff off
  , "Una serie que contiene un paralelo no prolijo emprolija el paralelo sin afectar la serie externa"
    ~: circuitoEmprolijado (Serie (Paralelo on (Serie cajaOn (Serie cajaOff cajaOn)) cajaOff off) cajaOn)
    ~?= Serie (Paralelo on (Serie (Serie cajaOn cajaOff) cajaOn) cajaOff off) cajaOn
  ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList
  [ "Dos cajas de distinto tipo tienen la misma estructura"
    ~: tienenLaMismaEstructura (cajaOn) (cajaOff)
    ~?= True
  , "Dos series con la misma estructura tienen la misma estructura"
    ~: tienenLaMismaEstructura (Serie cajaOn cajaOff) (Serie cajaNada cajaOff)
    ~?= True
  , "Dos palalelos con la misma estructura tienen la misma estructura"
    ~: tienenLaMismaEstructura (Paralelo on cajaOn cajaOff off) (Paralelo off cajaNada cajaNada on)
    ~?= True
  , "Una serie y una caja no tienen la misma estructura"
    ~: tienenLaMismaEstructura (Serie cajaOn cajaNada) (cajaOn)
    ~?= False
  , "Un paralelo y una caja no tienen la misma estructura"
    ~: tienenLaMismaEstructura (Paralelo off cajaOff cajaOff off) (cajaOff)
    ~?= False
  , "Una serie y un paralelo no tienen la misma estructura"
    ~: tienenLaMismaEstructura 
    (Serie cajaOn cajaOff) 
    (Paralelo on cajaNada cajaNada off)
    ~?= False
  , "Dos series con distinta estructura no tienen la misma estructura"
    ~: tienenLaMismaEstructura 
    (Serie cajaOn (Serie cajaOff cajaNada)) 
    (Serie (Serie cajaNada cajaOn) cajaOff)
    ~?= False
  , "Dos paralelos con distinta estructura no tienen la misma estructura"
    ~: tienenLaMismaEstructura 
    (Paralelo on  (Paralelo off cajaOff cajaOff off) cajaOff off) 
    (Paralelo off cajaNada (Paralelo on cajaNada cajaNada on) on)
    ~?= False
  , "Dos circuitos complejos con la misma estructura tienen la misma estructura"
    ~: tienenLaMismaEstructura 
    (Serie (Paralelo on  (Serie (Serie cajaOn  cajaOff)     cajaOn) cajaOff off) cajaOn) 
    (Serie (Paralelo off (Serie (Serie cajaOff cajaNada) cajaNada) cajaOff  on) cajaNada)
    ~?= True
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente = TestList 
  [ "El circuito más resistente de una caja es la misma caja"
    ~: subCircuitoMásResistente cajaOn
    ~?= cajaOn
  , "El circuito más resistente de una serie con dos cajas resistentes es la misma serie"
    ~: subCircuitoMásResistente (Serie cajaOff cajaOn)
    ~?= Serie cajaOff cajaOn
  , "El circuito más resistente de una serie con una caja resistente y otra no resistente es la misma serie (la resistencia de ambos subcircuitos es la misma)"
    ~: subCircuitoMásResistente (Serie cajaNada cajaOn)
    ~?= Serie cajaNada cajaOn
  , "El circuito más resistente de un paralelo con un solo subcircuito resistente es ese subcircuito"
    ~: subCircuitoMásResistente (Paralelo Nada cajaNada cajaOn Nada)
    ~?= cajaOn
  , "El circuito más resistente de un paralelo con dos subcircuitos resistentes es el más resistente de los dos"
    ~: subCircuitoMásResistente (Paralelo Nada cajaOn cajaOff off)
    ~?= cajaOn
  , "El circuito más resistente de un paralelo con dos cajas en entrada y salida es el mismo paralelo"
    ~: subCircuitoMásResistente (Paralelo on cajaOn cajaOff on)
    ~?= (Paralelo on cajaOn cajaOff on)
  ]

tests :: Test
tests = TestList
  [ TestLabel "invertido"                testsInvertido
  , TestLabel "hayCaminoIluminado"       testsHayCaminoIluminado
  , TestLabel "cantidadPrendidas"        testsCantidadPrendidas
  , TestLabel "cajasDeCircuito"          testsCajasDeCircuito
  , TestLabel "esCircuitoProlijo"        testsEsCircuitoProlijo
  , TestLabel "circuitoEmprolijado"      testsCircuitoEmprolijado
  , TestLabel "tienenLaMismaEstructura"  testsTienenLaMismaEstructura
  , TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
  ]

main :: IO ()
main = runTestTT tests >>= print
