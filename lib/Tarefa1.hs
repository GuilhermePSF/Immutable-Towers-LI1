-- |
-- Module      : Tarefa1
-- Description : Invariantes do Jogo
-- Copyright   : Lucas Faria Pinto <a111442@alunos.uminho.pt>
--               Guilherme Pinto da Silva Ferreira <a111042@alunos.uminho.pt>
--
--
-- Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
module Tarefa1 where

import Data.List
import Data.List.Utils
import LI12425

-- | Funcao que encontra o elemento na matriz usando
nLinhas :: Jogo -> Float
nLinhas mapa = fromIntegral $ length $ mapaJogo mapa

-- | Número de colunas do mapa.
nColunas :: Jogo -> Float
nColunas mapa = fromIntegral $ length $ head $ mapaJogo mapa

-- | Encontra o terreno de uma dada posição no mapa.
--
-- ==== Parâmetros
-- * @posicao@: A posição no formato (linha, coluna).
-- * @mapa@: O mapa onde será feita a busca.
--
-- ==== Resultado
-- Retorna o terreno (por exemplo, 'Terra', 'Relva') na posição especificada.
naPosicao :: Posicao -> [[a]] -> a
naPosicao (n, m) mapa = (mapa !! floor n) !! floor m

-- | Valida todas as invariantes do jogo.
validaJogo :: Jogo -> Bool
validaJogo jogo = validaPortais jogo && validaTorres jogo && validaInimigosPorLancar jogo && validaInimigosSobreTerra jogo && validavelocidadeInimigo jogo && validaprojeteisNormalizados jogo && validaBase jogo && validaInimigonaoSobrepostos jogo

-- | Verifica se os portais do jogo estão configurados corretamente.
--
-- ==== Critérios de validação
-- 1. Deve haver pelo menos um portal no jogo.
-- 2. Cada portal deve estar posicionado em um terreno válido ('Terra').
-- 3. Deve existir um caminho acessível de cada portal para a base.
-- 4. Os portais não podem estar sobrepostos com a base ou torres.
-- 5. No máximo uma onda pode estar ativa no tempo 0 em cada portal.
--
-- ==== Parâmetros
-- * @Jogo@: Estado do jogo contendo a base, portais, torres e o mapa.
--
-- ==== Resultado
-- Devolve 'True' se todas as condições acima forem satisfeitas, caso contrário, devolve 'False'.
validaPortais :: Jogo -> Bool
validaPortais jogo@(Jogo {baseJogo = base, portaisJogo = portais, torresJogo = torres, mapaJogo = mapa}) =
  existePortal
  && all bemPosicionados portais
  && existeCaminhoPortaisBase base
  && portalNaoSobreposto base
  && all unicaOndaAtiva portais
  where
    -- | Verifica se há pelo menos um portal na lista de portais.
    --
    -- ==== Parâmetros
    -- * @portais@: Lista de portais do jogo.
    --
    -- ==== Resultado
    -- Retorna 'True' se a lista de portais não estiver vazia; caso contrário, retorna 'False'.
    existePortal = not (null portais)

    -- | Verifica se um portal está posicionado em um terreno 'Terra' e dentro dos limites do mapa.
    --
    -- ==== Parâmetros
    -- * @Portal (n, m)@: A posição do portal no formato (linha, coluna).
    --
    -- ==== Resultado
    -- Retorna 'True' se o portal estiver posicionado em um terreno 'Terra' válido; caso contrário, retorna 'False'.
    bemPosicionados (Portal {posicaoPortal = (n, m)})
      | n >= 0 && m >= 0 && n < nLinhas jogo && m < nColunas jogo = naPosicao (n, m) mapa == Terra
      | otherwise = False

    -- | Verifica se existe um caminho entre cada portal e a base.
    --
    -- ==== Parâmetros
    -- * @Base@: Base do jogo.
    -- * @portais@: Lista de portais do jogo.
    -- * @mapa@: Mapa do jogo.
    --
    -- ==== Resultado
    -- Retorna 'True' se todos os portais têm um caminho válido até a base; caso contrário, retorna 'False'.
    existeCaminhoPortaisBase :: Base -> Bool
    existeCaminhoPortaisBase (Base {posicaoBase = posBase}) = all (elem posBase . caminhosApartir) (posicoesPortais portais)
      where
        -- | Gera todos os caminhos possíveis a partir de uma posição inicial.
        caminhosApartir :: Posicao -> [Posicao]
        caminhosApartir posInicial = bfs [posInicial] []
          where
            -- Busca em largura para explorar caminhos válidos.
            bfs :: [Posicao] -> [Posicao] -> [Posicao]
            bfs [] visitados = visitados
            bfs (e : encontrados) visitados =
              bfs (nub (encontrados ++ novosEncontrados)) (e : visitados)
              where
                novosEncontrados = filter (`notElem` visitados) (terrasAdjacentes e)

            -- Encontra todas as posições adjacentes de terreno 'Terra'.
            terrasAdjacentes :: Posicao -> [Posicao]
            terrasAdjacentes pos = filter (\x -> posicaoValida x && terrenoTerra) (posicoesAdjacentes pos)
              where
                terrenoTerra :: Bool
                terrenoTerra = naPosicao pos mapa == Terra

                posicaoValida :: Posicao -> Bool
                posicaoValida (n, m) = n >= 0 && m >= 0 && n < nLinhas jogo && m < nColunas jogo

                posicoesAdjacentes :: Posicao -> [Posicao]
                posicoesAdjacentes (n, m) = [(n - 1, m), (n + 1, m), (n, m - 1), (n, m + 1)]

    -- | Verifica se os portais não estão sobrepostos com a base ou torres.
    --
    -- ==== Parâmetros
    -- * @portais@: Lista de portais do jogo.
    -- * @torres@: Lista de torres do jogo.
    -- * @Base@: Base do jogo.
    --
    -- ==== Resultado
    -- Retorna 'True' se não houver sobreposições; caso contrário, retorna 'False'.
    portalNaoSobreposto
      (Base _ coordBase _) = not $ hasAny (posicoesPortais portais) (coordBase : posicoesTorres torres)

    posicoesPortais [] = []
    posicoesPortais ((Portal {posicaoPortal = coordPortais}) : ps) = coordPortais : posicoesPortais ps

    posicoesTorres [] = []
    posicoesTorres (Torre {posicaoTorre = coordTorre} : ts) = coordTorre : posicoesTorres ts

    -- | Verifica se há no máximo uma onda ativa no tempo 0 em cada portal.
    --
    -- ==== Parâmetros
    -- * @Portal@: Portal contendo suas ondas.
    --
    -- ==== Resultado
    -- Retorna 'True' se há no máximo uma onda ativa no tempo 0; caso contrário, retorna 'False'.
    unicaOndaAtiva (Portal _ ondas) = countElem 0 temposdeEntrada <= 1
      where
        temposdeEntrada = map entradaOnda ondas

-- | Verifica se as torres em um jogo são válidas.
-- Recebe um 'Jogo' e verifica que todas as condições de validade das torres foram satisfeitas.
--
-- === Critérios de Validação:
-- 1. As torres devem estar bem posicionadas dentro do mapa e numa posição de 'Relva'.
-- 2. O alcance das torres deve ser positivo ou igual a zero.
-- 3. A rajada das torres deve ser positiva.
-- 4. O ciclo das torres deve ser maior que zero.
-- 5. Nenhuma torre pode se sobrepor a outra.
validaTorres :: Jogo -> Bool
validaTorres jogo@(Jogo {torresJogo = torres}) =
  all bemPosicionados torres
    && all alcancePositivo torres
    && all rajadaPositiva torres
    && all cicloValido torres
    && naoSobrepostas torres
  where
    -- | Verifica se uma torre está dentro dos limites do mapa e posicionada em 'Relva'.
    bemPosicionados :: Torre -> Bool
    bemPosicionados (Torre {posicaoTorre = (n, m)}) = n >= 0 && m >= 0 && n < nLinhas jogo && m < nColunas jogo && naPosicao (n, m) (mapaJogo jogo) `elem` [Relva, Neve, Areia] -- Podemos incluir os terrenos novos?

    -- | Verifica se o alcance de uma torre é positivo ou igual a zero.
    alcancePositivo :: Torre -> Bool
    alcancePositivo (Torre {alcanceTorre = alcance}) = alcance >= 0

    -- | Verifica se a rajada de uma torre é positiva.
    rajadaPositiva :: Torre -> Bool
    rajadaPositiva (Torre {rajadaTorre = rajada}) = rajada > 0 -- FIX game towers

    -- | Verifica se o ciclo de uma torre é maior que zero.
    cicloValido :: Torre -> Bool
    cicloValido (Torre {cicloTorre = ciclo}) = ciclo > 0 -- FIX game towers

    -- | Verifica se nenhuma torre está sobreposta a outra.
    naoSobrepostas :: [Torre] -> Bool
    naoSobrepostas torre = length (posicoesTorre torre) == length (nub $ posicoesTorre torre)
      where
        -- | Retorna as posições de todas as torres.
        posicoesTorre :: [Torre] -> [Posicao]
        posicoesTorre [] = []
        posicoesTorre ((Torre {posicaoTorre = pos}) : ts) = pos : posicoesTorre ts

-- | Verifica se todos os inimigos por lançar são válidos.
-- Recebe um 'Jogo' e verifica que todas as condições de validade dos inimigos por lançar foram satisfeitas.
--
-- === Critérios de Validação:
-- 1. Todos os inimigos devem estar na posição do portal.
-- 2. A vida dos inimigos deve ser maior que zero.
-- 3. Os inimigos não devem ter projéteis.
validaInimigosPorLancar :: Jogo -> Bool
validaInimigosPorLancar jogo = validaPortaisInimigos (portaisJogo jogo)
  where
    -- | Verifica se todos os portais têm ondas de inimigos válidas.
    validaPortaisInimigos :: [Portal] -> Bool
    validaPortaisInimigos [] = True
    validaPortaisInimigos (portal : resto) =
      validaOndas (posicaoPortal portal) (ondasPortal portal) && validaPortaisInimigos resto

    -- | Verifica se todas as ondas de um portal são válidas.
    validaOndas :: Posicao -> [Onda] -> Bool
    validaOndas _ [] = True
    validaOndas posPortal (onda : resto) =
      validaInimigos posPortal (inimigosOnda onda) && validaOndas posPortal resto

    -- | Verifica se todos os inimigos de uma onda são válidos.
    validaInimigos :: Posicao -> [Inimigo] -> Bool
    validaInimigos _ [] = True
    validaInimigos posPortal (inimigo : resto) =
      validaInimigo posPortal inimigo && validaInimigos posPortal resto

    -- | Verifica se um inimigo é válido.
    validaInimigo :: Posicao -> Inimigo -> Bool
    validaInimigo posPortal inimigo =
      posicaoInimigo inimigo == posPortal
        && vidaInimigo inimigo > 0
        && null (projeteisInimigo inimigo)

-- | Verifica se todos os inimigos em jogo estão sobre terrenos de terra.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se todas as condições de validade dos inimigos sobre terra forem satisfeitas; caso contrário, retorna 'False'.
--
-- === Critérios de Validação:
-- 1. Todos os inimigos devem estar sobre terrenos de terra.
validaInimigosSobreTerra :: Jogo -> Bool
validaInimigosSobreTerra jogo = validaInimigos (inimigosJogo jogo)
  where
    -- | Verifica se todos os inimigos estão sobre terrenos de terra.
    validaInimigos :: [Inimigo] -> Bool
    validaInimigos [] = True
    validaInimigos (inimigo : resto) =
      estaSobreTerra (posicaoInimigo inimigo) (mapaJogo jogo) && validaInimigos resto

    -- | Verifica se uma posição está sobre um terreno de terra.
    estaSobreTerra :: Posicao -> Mapa -> Bool
    estaSobreTerra (x, y) mapa =
      dentroDoMapa (floor y) (floor x) mapa && mapa !! floor y !! floor x == Terra -- floor x = coluna | floor y = linha

    -- | Verifica se uma posição está dentro dos limites do mapa.
    dentroDoMapa :: Int -> Int -> Mapa -> Bool
    dentroDoMapa linha coluna mapa =
      linha >= 0
        && linha < length mapa
        && coluna >= 0
        && coluna < length (head mapa)

-- | Verifica se os inimigos não estão sobrepostos com as torres.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se todas as condições de validade dos inimigos não sobrepostos forem satisfeitas; caso contrário, retorna 'False'.
--
-- === Critérios de Validação:
-- 1. Nenhum inimigo deve estar sobreposto com uma torre.
validaInimigonaoSobrepostos :: Jogo -> Bool
validaInimigonaoSobrepostos jogo = verificaInimigos (inimigosJogo jogo)
  where
    -- | Verifica se os inimigos não estão sobrepostos com as torres.
    verificaInimigos :: [Inimigo] -> Bool
    verificaInimigos [] = True
    verificaInimigos (inimigo : resto) =
      not (estaSobreposto (posicaoInimigo inimigo) (torresJogo jogo) && verificaInimigos resto)

    -- | Verifica se um inimigo está sobreposto com alguma torre.
    estaSobreposto :: Posicao -> [Torre] -> Bool
    estaSobreposto _ [] = False
    estaSobreposto posicaoInimigo (torre : restoTorres) = mesmaposicao posicaoInimigo (posicaoTorre torre) || estaSobreposto posicaoInimigo restoTorres

    -- | Verifica se duas posições são iguais.
    mesmaposicao :: Posicao -> Posicao -> Bool
    mesmaposicao (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

-- | Verifica se a velocidade dos inimigos é válida.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se todas as condições de validade das velocidades dos inimigos forem satisfeitas; caso contrário, retorna 'False'.
--
-- === Critérios de Validação:
-- 1. A velocidade de cada inimigo deve ser maior ou igual a zero.
validavelocidadeInimigo :: Jogo -> Bool
validavelocidadeInimigo jogo = verificaVelocidades (inimigosJogo jogo)
  where
    verificaVelocidades :: [Inimigo] -> Bool
    verificaVelocidades [] = True
    verificaVelocidades (inimigo : restoInimigos) = velocidadeInimigo inimigo >= 0 && verificaVelocidades restoInimigos

-- | Verifica se os projéteis estão normalizados.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se todas as condições de validade dos projéteis forem satisfeitas; caso contrário, retorna 'False'.
--
-- === Critérios de Validação:
-- 1. Não deve haver projéteis duplicados.
-- 1. Não deve haver projéteis incompatíveis (Fogo e Resina, Fogo e Gelo).
validaprojeteisNormalizados :: Jogo -> Bool
validaprojeteisNormalizados jogo = verificaInimigos $ inimigosJogo jogo
  where
    -- | Verifica se a velocidade dos inimigos é válida.
    verificaInimigos :: [Inimigo] -> Bool
    verificaInimigos [] = True
    verificaInimigos (inimigo : resto) =
      listaNormalizada (projeteisInimigo inimigo) && verificaInimigos resto

    -- | Verifica se a lista de projéteis está normalizada.
    listaNormalizada :: [Projetil] -> Bool
    listaNormalizada projeteis = semDuplicados (tiposProjeteis projeteis) && semIncompativeis (tiposProjeteis projeteis)
      where
        tiposProjeteis = map tipoProjetil

    -- | Verifica se não há projéteis duplicados.
    semDuplicados :: [TipoProjetil] -> Bool
    semDuplicados [] = True
    semDuplicados (x : xs) = notElem x xs && semDuplicados xs

    -- | Verifica se não há projéteis incompatíveis.
    semIncompativeis :: [TipoProjetil] -> Bool
    semIncompativeis projeteis = not (Fogo `elem` projeteis && Resina `elem` projeteis) && not (Fogo `elem` projeteis && Gelo `elem` projeteis)

-- | Verifica se a base está em uma posição válida.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se todas as condições de validade da base forem satisfeitas; caso contrário, retorna 'False'.
--
-- === Critérios de Validação:
-- 1. A base deve estar sobre um terreno de terra.
-- 2. A base não deve possuir créditos negativos.
-- 3. A base não deve estar sobreposta com torres ou portais.
validaBase :: Jogo -> Bool
validaBase jogo =
  baseSobreTerra (posicaoBase (baseJogo jogo)) (mapaJogo jogo)
    && semCreditoNegativo (creditosBase (baseJogo jogo))
    && semSobreposicao (posicaoBase (baseJogo jogo)) (torresJogo jogo) (portaisJogo jogo)
  where
    -- | Verifica se a base está sobre um terreno de terra.
    baseSobreTerra :: Posicao -> Mapa -> Bool
    baseSobreTerra pos mapa = naPosicao pos mapa == Terra

    -- | Verifica se a base não possui créditos negativos.
    semCreditoNegativo :: Creditos -> Bool
    semCreditoNegativo creditos = creditos >= 0

    -- | Verifica se a base não está sobreposta com torres ou portais.
    semSobreposicao :: Posicao -> [Torre] -> [Portal] -> Bool
    semSobreposicao posicao torres portais =
      not (posicaoEmTorres posicao torres) && not (posicaoEmPortais posicao portais)

    -- | Verifica se uma posição está sobreposta com alguma torre.
    posicaoEmTorres :: Posicao -> [Torre] -> Bool
    posicaoEmTorres _ [] = False
    posicaoEmTorres posicao (torre : resto) =
      posicao == posicaoTorre torre || posicaoEmTorres posicao resto

    -- | Verifica se uma posição está sobreposta com algum portal.
    posicaoEmPortais :: Posicao -> [Portal] -> Bool
    posicaoEmPortais _ [] = False
    posicaoEmPortais posicao (portal : resto) =
      posicao == posicaoPortal portal || posicaoEmPortais posicao resto
