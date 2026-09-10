module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito
recCircuito ::
    (Caja -> b) ->
    (Circuito -> Circuito -> b -> b -> b) ->
    (Circuito -> Circuito -> Caja -> b -> b -> Caja -> b) ->
    Circuito ->
    b
recCircuito cCaja cSerie cParalelo c =
    case c of
        Caja caja -> cCaja caja
        Serie circuitoInicial circuitoFinal -> cSerie circuitoInicial circuitoFinal (rec circuitoInicial) (rec circuitoFinal)
        Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida -> cParalelo circuitoDerecho circuitoIzquierdo cajaEntrada (rec circuitoIzquierdo) (rec circuitoDerecho) cajaSalida
    where
        rec = recCircuito cCaja cSerie cParalelo

-- 2: foldCircuito

foldCircuito ::
    (Caja -> b) ->
    (b -> b -> b) ->
    (Caja -> b -> b -> Caja -> b) ->
    Circuito ->
    b
foldCircuito cCaja cSerie cParalelo = recCircuito cCaja (const.const$cSerie) (const.const$cParalelo)

-- 3 invertido
invertido :: Circuito -> Circuito
invertido = foldCircuito Caja (flip$Serie) paraleloInvertido
    where
        paraleloInvertido cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida = Paralelo cajaSalida resultadoDerecho resultadoIzquierdo cajaEntrada

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado = foldCircuito cCaja (&&) cParalelo
  where
    cCaja (Bombilla True) = True
    cCaja _               = False
    cParalelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      cCaja cajaEntrada && (resultadoIzquierdo || resultadoDerecho) && cCaja cajaSalida

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas = foldCircuito cCaja (+) cParalelo
  where
    cCaja (Bombilla True) = 1
    cCaja _               = 0
    cParalelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      cCaja cajaEntrada + resultadoIzquierdo + resultadoDerecho + cCaja cajaSalida

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = foldCircuito cCaja (++) cParalelo
  where
    cCaja c = [c]
    cParalelo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      [cajaEntrada] ++ resultadoIzquierdo ++ resultadoDerecho ++ [cajaSalida]

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito (const$True) cSerie cParalelo
  where
    cSerie _ (Serie _ _) _ _ = False
    cSerie _ _ resultadoInicial resultadoFinal = resultadoInicial && resultadoFinal
    cParalelo _ _ _ resultadoIzquierdo resultadoDerecho _ = resultadoIzquierdo && resultadoDerecho

-- 8: circuitoEmprolijado

circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado = foldCircuito Caja cSerie Paralelo
  where
    cSerie resultadoInicial resultadoFinal = serieRotada resultadoInicial resultadoFinal
    serieRotada circuitoInicial (Serie circuitoIzquierdo circuitoDerecho) =
      serieRotada (serieRotada circuitoInicial circuitoIzquierdo) circuitoDerecho
    serieRotada circuitoInicial circuitoFinal = Serie circuitoInicial circuitoFinal

-- 9: tienenLaMismaEstructura

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura = foldCircuito cCaja cSerie cParalelo
  where
    cCaja _ (Caja _) = True
    cCaja _ _        = False

    cSerie resultadoInicial resultadoFinal (Serie circuitoInicial2 circuitoFinal2) =
      resultadoInicial circuitoInicial2 && resultadoFinal circuitoFinal2
    cSerie _ _ _ = False

    cParalelo _ resultadoIzquierdo resultadoDerecho _ (Paralelo _ circuitoIzquierdo2 circuitoDerecho2 _) =
      resultadoIzquierdo circuitoIzquierdo2 && resultadoDerecho circuitoDerecho2
    cParalelo _ _ _ _ _ = False

-- 10: subCircuitoMásResistente

resistenciaCircuito :: Circuito -> Float
resistenciaCircuito = undefined

circuitoMásResistente :: Circuito -> Circuito -> Circuito
circuitoMásResistente circuito1 circuito2 = if resistenciaCircuito circuito1 >= resistenciaCircuito circuito2 
  then circuito1 else circuito2

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = recCircuito Caja cSerie cParalelo
  where
    cSerie circuitoInicial circuitoFinal resultadoInicial resultadoFinal =
      circuitoMásResistente (Serie circuitoInicial circuitoFinal) (circuitoMásResistente resultadoInicial resultadoFinal)
    cParalelo circuitoDerecho circuitoIzquierdo cajaEntrada resultadoIzquierdo resultadoDerecho cajaSalida =
      circuitoMásResistente (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) (circuitoMásResistente resultadoIzquierdo resultadoDerecho)

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

{AUX} cajaAlternada (cajaAlternada caja) = caja

demostración de {AUX}
cajaAlternada (cajaAlternada caja) = caja

lema de generación de Caja: casos Nada, Bombilla b

caso caja = Nada:
cajaAlternada (cajaAlternada Nada) = Nada
{CAN}
cajaAlternada Nada = Nada
{CAN}
Nada = Nada

caso caja = Bombilla b:
cajaAlternada (cajaAlternada (Bombilla b)) = Bombilla b
{CAB}
cajaAlternada (Bombilla not b) = Bombilla b

Lema de generación sobre booleanos: casos True, Flase

caso b = True:
cajaAlternada (Bombilla not True) = Bombilla True
{NT}
cajaAlternada (Bombilla False) = Bombilla True
{CAB}
Bombilla not False = Bombilla True
{NF}
Bombilla True = Bombilla True

caso b = False:
cajaAlternada (Bombilla not False) = Bombilla False
{NF}
cajaAlternada (Bombilla True) = Bombilla False
{CAB}
Bombilla not True = Bombilla False
{NT}
Bombilla False = Bombilla False


--
qvq parat todo circ::Circuito: alternado . alternado circ = id circ
P(circ) = alternado . alternado circ = id circ
{EXT} 
alterando.alternado circ = id circ

Inducción sobre Circuito
Lema de generacion de Circuito: casos Caja, Serie, Paralelo

P(Caja caja)
P(circ) => P(Serie ci cf)
P(circ) => p(Paralelo ce ci cd cs)

{HI} P(Circ)

Caso base P(Caja caja)
alterando.alternado (Caja caja) = id (Caja caja)
{C}
alternado (alternado (Caja caja)) = id (Caja caja)
{AC}
alternado (Caja (cajaAlternada caja)) = id (Caja caja)
{AC}
Caja (cajaAlternada (cajaAlternada caja)) = id (Caja caja)
{AUX}
Caja caja = id (Caja caja)
{I}
id (Caja caja) = id (Caja caja)

caso P(Serie ci cf)
alterando.alternado (Serie ci cf) = id (Serie ci cf)
{C}
alternado (alternado (Serie ci cf)) = id (Serie ci cf)
{AS}
alternado (Serie (alternado ci) (alternado cf)) = id (Serie ci cf)
{AS}
Serie (alternado (alternado ci)) (alternado (alternado cf)) = id (Serie ci cf)
{C}x2
Serie (alternado.alternado ci) (alternado.alternado cf) = id (Serie ci cf)
{HI}
Serie (id ci) (id cf) = id (Serie ci cf)
{I}x2
Serie ci cf = id (Serie ci cf)
{I}
id (Serie ci cf) = id (Serie ci cf)

caso P(Paralelo ce ci cd cs):
alterando.alternado (Paralelo ce ci cd cs)) = id (Paralelo ce ci cd cs)
{C}
alternado (alternado (Paralelo ce ci cd cs)) = id (Paralelo ce ci cd cs)
{AP}
alternado (Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)) = id (Paralelo ce ci cd cs)
{AP}
Paralelo (cajaAlternada (cajaAlternada ce)) (alternado (alternado ci)) (alternado (alternado cd)) (cajaAlternada (cajaAlternada cs)) = id (Paralelo ce ci cd cs)
{C}x2
Paralelo (cajaAlternada (cajaAlternada ce)) (alternado.alternado ci) (alternado.alternado cd) (cajaAlternada (cajaAlternada cs)) = id (Paralelo ce ci cd cs)
{HI}
Paraleo (cajaAlternada (cajaAlternada ce)) (id ci) (id cd) (cajaAlternada (cajaAlternada cs)) = id (Paralelo ce ci cd cs)
{I}x2
Paraleo (cajaAlternada (cajaAlternada ce)) ci cd (cajaAlternada (cajaAlternada cs)) = id (Paralelo ce ci cd cs)
{AUX}x2 
Paralelo ce ci cd cs = id (Paralelo ce ci cd cs)
{I}
id (Paralelo ce ci cd cs) = id (Paralelo ce ci cd cs)

--}
