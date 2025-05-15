-- |
-- Module      : Tarefa3
-- Description : Mecânica do Jogo
-- Copyright   : Lucas Faria Pinto <a111442@alunos.uminho.pt>
--               Guilherme Pinto da Silva Ferreira <a111042@alunos.uminho.pt>
--
--
-- Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
module Tarefa3 where

import LI12425
import Tarefa1
import Tarefa2

-- | Atualiza o estado do jogo dado um intervalo de tempo.
-- Esta função aplica várias atualizações sequenciais ao estado do jogo, incluindo cooldowns, movimentos de inimigos, créditos, danos e disparos.
--
-- ==== Parâmetros:
-- * Tempo - O intervalo de tempo a ser considerado para a atualização.
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O novo estado do jogo após a aplicação das atualizações.
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tick jogo = atualizaCooldown tick $ moveInimigos tick $ darCreditoInimigosVivos $ danoEfeitoProjeteis tick $ disparaTorres $ danoBase $ spawnInimigos $ removeProjeteisExpirados jogo

-- | Atualiza o cooldown de torres e projéteis no jogo.
-- Esta função decrementa o tempo de cooldown das torres e dos projéteis dos inimigos.
--
-- ==== Parâmetros:
-- * Tempo - O intervalo de tempo a ser considerado para a atualização.
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O estado do jogo com os cooldowns atualizados.
atualizaCooldown :: Tempo -> Jogo -> Jogo
atualizaCooldown tick jogo@(Jogo {torresJogo = torres, inimigosJogo = inimigos, portaisJogo = portais, lasersJogo = lasers}) =
  (jogo {torresJogo = atualizaTorre, portaisJogo = map atualizaOndas portais, inimigosJogo = map (atualizaProjeteisInimigo . atualizaDistanciaInimigos) inimigos, lasersJogo = lasersAtualizados})
  where
    -- | Atualiza o cooldown de uma torre.
    atualizaTorre :: [Torre]
    atualizaTorre =
      filter (\torre -> tempoTorre torre <= 0) torres
        ++ map (\torre@(Torre {tempoTorre = tempo}) -> torre {tempoTorre = tempo - tick}) (filter (\torre -> tempoTorre torre > 0) torres)

    -- | Atualiza os projéteis de um inimigo.
    atualizaProjeteisInimigo :: Inimigo -> Inimigo
    atualizaProjeteisInimigo inimigo@(Inimigo {projeteisInimigo = projeteis}) =
      inimigo {projeteisInimigo = map atualizaProjetil projeteis}

    -- | Atualiza o cooldown de um projétil.
    atualizaProjetil :: Projetil -> Projetil
    atualizaProjetil projetil@(Projetil {duracaoProjetil = Infinita}) = projetil
    atualizaProjetil projetil@(Projetil {duracaoProjetil = Finita tempo}) = projetil {duracaoProjetil = Finita (tempo - tick)}

    -- | Atualiza as ondas de um portal.
    atualizaOndas :: Portal -> Portal
    atualizaOndas portal = portal {ondasPortal = novasOndas}
      where
        novasOndas = concatMap atualizaOnda $ ondasPortal portal

        -- | Atualiza uma onda com base no tempo de tick.
        atualizaOnda :: Onda -> [Onda]
        atualizaOnda onda
          -- Caso 1: Se o tempo de entrada da onda for maior que 0, reduz o tempo de entrada pelo tempo de tick.
          -- Isto significa que a onda ainda não está ativa e estamos à espera para que ela comece.
          | entradaOnda onda > 0 = [onda {entradaOnda = entradaOnda onda - tick}]
          -- Caso 2: Se o tempo de ciclo da onda for maior que 0, reduz o tempo de ciclo pelo tempo de tick.
          -- Isso significa que estamos à espera o próximo inimigo ser lançado.
          | tempoOnda onda > 0 = [onda {tempoOnda = tempoOnda onda - tick}]
          -- Caso 3: Se a lista de inimigos da onda estiver vazia, não há mais inimigos para lançar.
          -- Nesse caso, simplesmente retornamos as listas inalteradas.
          | null (inimigosOnda onda) = []
          -- Caso 4: Caso contrário, reinicia o tempo de ciclo.
          -- Isso significa que o tempo de ciclo chegou a zero e há inimigos na lista para serem lançados.
          | otherwise = [onda {tempoOnda = cicloOnda onda}]

    -- | Atualiza o parametro distanciaRestante de um Inimigo
    atualizaDistanciaInimigos :: Inimigo -> Inimigo
    atualizaDistanciaInimigos inimigo@(Inimigo {spriteDistancia = distanciaRestante, spritesInimigo = sprites, projeteisInimigo = projeteis, velocidadeInimigo = vel})
      | distanciaRestante <= 0 = inimigo {spriteDistancia = spriteCiclo inimigo, spritesInimigo = tail sprites ++ take 1 sprites}
      | otherwise = inimigo {spriteDistancia = distanciaRestante - tick * velPosProjeteis projeteis vel}

    -- | Atualiza os Lasers do Jogo diminuindo o tempoLaser e removendo os Lasers com tempoLaser menor ou igual a zero
    lasersAtualizados :: [Laser]
    lasersAtualizados = filter (\laser -> tempoLaser laser > 0) $ map (\laser@(Laser {tempoLaser = tempo}) -> laser {tempoLaser = tempo - tick}) lasers

-- | Spawna novos inimigos no jogo com base nas ondas dos portais.
--
-- ==== Parâmetros:
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O novo estado do jogo com os novos inimigos adicionados.
spawnInimigos :: Jogo -> Jogo
spawnInimigos jogo@(Jogo {portaisJogo = portais, inimigosJogo = inimigos}) = jogo {portaisJogo = map atualizarInimigosOnda portais, inimigosJogo = inimigosALancar jogo ++ inimigos}

-- | Atualiza as ondas de um portal removendo os inimigos lançados.
--
-- ==== Parâmetros:
-- * Portal - O portal cujas ondas serão atualizadas.
--
-- ==== Resultado:
-- O portal atualizado com as ondas modificadas.
atualizarInimigosOnda :: Portal -> Portal
atualizarInimigosOnda portal@(Portal {ondasPortal = ondas}) =
  portal
    { ondasPortal =
        map retirarInimigoLancado (filter (\onda -> entradaOnda onda <= 0 && tempoOnda onda <= 0 && not (null (inimigosOnda onda))) ondas)
          ++ filter (\onda -> entradaOnda onda > 0 || tempoOnda onda > 0) ondas
    }

-- | Remove o primeiro inimigo da lista de uma onda, representando o lançamento de um inimigo.
--
-- ==== Parâmetros:
-- * Onda - A onda da qual o inimigo será removido.
--
-- ==== Resultado:
-- A onda atualizada com o primeiro inimigo removido.
retirarInimigoLancado :: Onda -> Onda
retirarInimigoLancado onda@(Onda {inimigosOnda = inimigos}) = onda {inimigosOnda = tail inimigos}

-- | Determina os inimigos prontos para serem lançados a partir dos portais.
--
-- ==== Parâmetros:
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- A lista de inimigos prontos para serem lançados.
inimigosALancar :: Jogo -> [Inimigo]
inimigosALancar (Jogo {portaisJogo = portais}) = concatMap inimigoALancar portais

-- | Determina os inimigos de um portal prontos para lançamento.
--
-- ==== Parâmetros:
-- * Portal - O portal cujos inimigos serão determinados para lançamento.
--
-- ==== Resultado:
-- A lista de inimigos prontos para lançamento a partir do portal.
inimigoALancar :: Portal -> [Inimigo]
inimigoALancar (Portal {ondasPortal = ondas}) = take 1 $ concatMap inimigosOnda $ filter (\onda -> entradaOnda onda <= 0 && tempoOnda onda <= 0 && not (null $ inimigosOnda onda)) ondas

-- | Atualiza os créditos da base com base nos inimigos derrotados.
--
-- ==== Parâmetros:
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O novo estado do jogo com os créditos da base atualizados e com os inimigos derrotados removidos.
darCreditoInimigosVivos :: Jogo -> Jogo
darCreditoInimigosVivos jogo@(Jogo {baseJogo = base})
  | creditosBase base + creditosAdicionais >= 9999 = jogo {inimigosJogo = inimigosVivos, baseJogo = base {creditosBase = 9999}}
  | otherwise = jogo {inimigosJogo = inimigosVivos, baseJogo = base {creditosBase = creditosBase base + creditosAdicionais}}
  where
    -- | Filtra inimigos vivos.
    inimigosVivos = filter (\inimigo -> vidaInimigo inimigo > 0) (inimigosJogo jogo)
    -- | Calcula créditos adicionais baseados nos inimigos derrotados.
    creditosAdicionais = sum . map butimInimigo $ filter (\inimigo -> vidaInimigo inimigo <= 0) (inimigosJogo jogo)

-- | Aplica dano à base do jogador por inimigos que chegaram até ela.
--
-- ==== Parâmetros:
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O novo estado do jogo com a vida da base atualizada e os inimigos que causaram dano removidos.
danoBase :: Jogo -> Jogo
danoBase jogo@(Jogo {baseJogo = base}) =
  jogo
    { baseJogo = (base {vidaBase = vidaBase base - danoABase}),
      inimigosJogo = filter (`notElem` inimigosNaBase) (inimigosJogo jogo)
    }
  where
    -- | Calcula o dano causado pelos inimigos à base.
    danoABase :: Float
    danoABase = sum $ map ataqueInimigo inimigosNaBase

    -- | Determina os inimigos que alcançaram a base.
    inimigosNaBase = filter (\inimigo -> floorPar (posicaoInimigo inimigo) == floorPar (posicaoBase base)) (inimigosJogo jogo)

    -- | Arredonda as coordenadas de uma posição para inteiros.
    floorPar (a, b) = (floor a :: Int, floor b :: Int)

-- | Dispara torres contra inimigos dentro do alcance.
--
-- ==== Parâmetros:
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O novo estado do jogo com as torres a disparar contra os inimigos.
disparaTorres :: Jogo -> Jogo
disparaTorres jogo@(Jogo {torresJogo = torres, inimigosJogo = inimigos, lasersJogo = lasersAtuais}) =
  jogo
    { torresJogo = map (\torre -> torre {tempoTorre = cicloTorre torre}) torresADisparar ++ torresNaoADisparar,
      inimigosJogo = disparaTorresAux torresADisparar inimigos,
      lasersJogo = lasersAtuais ++ concatMap (lasers inimigos) torresADisparar
    }
  where
    -- | Adiciona Lasers ao Jogo.
    lasers :: [Inimigo] -> Torre -> [Laser]
    lasers inimigosAtivos torre@(Torre {posicaoTorre = posTorre, rajadaTorre = rajada, projetilTorre = (Projetil {tipoProjetil = tipo})}) =
      map ((\posInimigo -> (Laser {posicaoTorreLaser = posTorre, posicaoInimigoLaser = posInimigo, tipoLaser = tipo, tempoLaser = 0.5})) . posicaoInimigo) (take rajada (filter (estaNoAlcance torre) inimigosAtivos))

    -- | Filtra torres prontas para disparar.
    torresADisparar = filter (\torre -> tempoTorre torre <= 0 && any (estaNoAlcance torre) inimigos) torres
    torresNaoADisparar = filter (\torre -> tempoTorre torre > 0 || not (any (estaNoAlcance torre) inimigos)) torres

-- | Processa disparos de torres em inimigos.
--
-- ==== Parâmetros:
-- * [Torre] - A lista de torres que irão disparar.
-- * [Inimigo] - A lista de inimigos.
--
-- ==== Resultado:
-- A lista de inimigos atualizada após os disparos das torres.
disparaTorresAux :: [Torre] -> [Inimigo] -> [Inimigo]
disparaTorresAux ts inimigos =
  foldl
    ( \inmgs torre ->
        dispara (torre, rajadaTorre torre) inmgs [] []
    )
    inimigos
    ts

-- | Processa disparos individuais de uma torre em inimigos.
--
-- ==== Parâmetros:
-- * (Torre, Int) - A torre e o número de disparos restantes.
-- * [Inimigo] - A lista de inimigos ativos.
-- * [Inimigo] - A lista de inimigos já disparados.
-- * [Inimigo] - A lista de inimigos fora do alcance.
--
-- ==== Resultado:
-- A lista de inimigos atualizada após os disparos da torre.
dispara :: (Torre, Int) -> [Inimigo] -> [Inimigo] -> [Inimigo] -> [Inimigo]
dispara _ [] [] [] = []
dispara _ [] [] naoNoAlcance = naoNoAlcance
dispara (torre, disparosRestantes) [] disparados naoNoAlcance
  | disparosRestantes > 0 = dispara (torre, disparosRestantes) disparados [] naoNoAlcance
  | otherwise = disparados ++ naoNoAlcance
dispara (_, 0) inimigosAtivos disparados naoNoAlcance = disparados ++ inimigosAtivos ++ naoNoAlcance
dispara (torre, disparosRestantes) (inimigo : is) disparados naoNoAlcance
  | estaNoAlcance torre inimigo =
      dispara
        (torre, disparosRestantes - 1)
        is
        (atingeInimigo torre inimigo : disparados)
        naoNoAlcance
  | otherwise =
      dispara
        (torre, disparosRestantes)
        is
        disparados
        (inimigo : naoNoAlcance)

-- | Verifica se um inimigo está no alcance de uma torre.
estaNoAlcance :: Torre -> Inimigo -> Bool
estaNoAlcance torre inimigo = dist (posicaoInimigo inimigo) (posicaoTorre torre) <= alcanceTorre torre

-- | Aplica dano e efeitos de projéteis aos inimigos.
danoEfeitoProjeteis :: Tempo -> Jogo -> Jogo
danoEfeitoProjeteis tick jogo@(Jogo {inimigosJogo = inimigos}) = jogo {inimigosJogo = map danoEfeitoProjeteisAux inimigos}
  where
    -- | Aplica dano e efeitos de projéteis em um único inimigo.
    danoEfeitoProjeteisAux :: Inimigo -> Inimigo
    danoEfeitoProjeteisAux inimigo@(Inimigo {vidaInimigo = vida, projeteisInimigo = projeteis})
      | Fogo `elem` projeteisAtivos = inimigo {vidaInimigo = vida - tick * danoFogo}
      | Veneno `elem` projeteisAtivos = inimigo {vidaInimigo = vida - tick * danoVeneno}
      | otherwise = inimigo
      where
        projeteisAtivos = map tipoProjetil projeteis
        danoFogo = 10
        danoVeneno = 5

-- | Remove projéteis que já expiraram de inimigos.
-- Esta função aplica os danos e efeitos dos projéteis ativos nos inimigos ao longo de um intervalo de tempo.
--
-- ==== Parâmetros:
-- * Tempo - O intervalo de tempo a ser considerado para a aplicação dos danos e efeitos.
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O novo estado do jogo com os danos e efeitos dos projéteis aplicados aos inimigos.
removeProjeteisExpirados :: Jogo -> Jogo
removeProjeteisExpirados jogo@(Jogo {inimigosJogo = inimigosAtivos}) = jogo {inimigosJogo = map removeProjeteisExpiradosAux inimigosAtivos}
  where
    -- | Remove projéteis expirados de um único inimigo.
    removeProjeteisExpiradosAux :: Inimigo -> Inimigo
    removeProjeteisExpiradosAux inimigo =
      inimigo
        { projeteisInimigo =
            projeteisInfinitos ++ filter (\projetil -> duracaoProjetil projetil > Finita 0) projeteisFinitos
        }
      where
        projeteisInfinitos = filter (\i -> duracaoProjetil i == Infinita) (projeteisInimigo inimigo)
        projeteisFinitos = filter (\i -> duracaoProjetil i /= Infinita) (projeteisInimigo inimigo)

-- | Calcula a distância entre duas posições.
dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

-- | Move todos os inimigos no jogo, atualizando suas posições e direções.
-- Esta função utiliza `moveInimigoAux` para atualizar a posição e direção de cada inimigo com base no mapa direcional.
--
-- ==== Parâmetros:
-- * Tempo - O intervalo de tempo a ser considerado para a atualização.
-- * Jogo - O estado atual do jogo.
--
-- ==== Resultado:
-- O novo estado do jogo com as posições e direções dos inimigos atualizadas.
moveInimigos :: Tempo -> Jogo -> Jogo
moveInimigos tick jogo@(Jogo {mapaDirecaoJogo = mapaDir, inimigosJogo = inimigos}) = jogo {inimigosJogo = map (moveInimigoAux tick mapaDir) inimigos}

-- | Move um único inimigo, atualizando sua posição e direção com base no mapa.
--
-- ==== Parâmetros:
-- * Tempo - O intervalo de tempo a ser considerado para a atualização.
-- * MapaDirecional - O mapa que indica as direções.
-- * Inimigo - O inimigo cuja posição e direção serão atualizadas.
--
-- ==== Resultado:
-- O inimigo com a posição e direção atualizadas.
-- Norte
moveInimigoAux :: Tempo -> MapaDirecional -> Inimigo -> Inimigo
moveInimigoAux tick mapaDir inimigo@(Inimigo {velocidadeInimigo = vel, posicaoInimigo = (posLinha, posColuna), projeteisInimigo = projeteis, direcaoInimigo = Norte}) =
  inimigo {posicaoInimigo = (posLinha - tick * velPosProjeteis projeteis vel, posColuna), direcaoInimigo = mudarDirecao mapaDir inimigo}
-- Sul
moveInimigoAux tick mapaDir inimigo@(Inimigo {velocidadeInimigo = vel, posicaoInimigo = (posLinha, posColuna), projeteisInimigo = projeteis, direcaoInimigo = Sul}) =
  inimigo {posicaoInimigo = (posLinha + tick * velPosProjeteis projeteis vel, posColuna), direcaoInimigo = mudarDirecao mapaDir inimigo}
-- Este
moveInimigoAux tick mapaDir inimigo@(Inimigo {velocidadeInimigo = vel, posicaoInimigo = (posLinha, posColuna), projeteisInimigo = projeteis, direcaoInimigo = Este}) =
  inimigo {posicaoInimigo = (posLinha, posColuna + tick * velPosProjeteis projeteis vel), direcaoInimigo = mudarDirecao mapaDir inimigo}
-- Oeste
moveInimigoAux tick mapaDir inimigo@(Inimigo {velocidadeInimigo = vel, posicaoInimigo = (posLinha, posColuna), projeteisInimigo = projeteis, direcaoInimigo = Oeste}) =
  inimigo {posicaoInimigo = (posLinha, posColuna - tick * velPosProjeteis projeteis vel), direcaoInimigo = mudarDirecao mapaDir inimigo}
-- Repouso
moveInimigoAux _ _ inimigo@(Inimigo {posicaoInimigo = (posLinha, posColuna), direcaoInimigo = Repouso}) =
  inimigo {posicaoInimigo = (posLinha, posColuna), direcaoInimigo = Repouso}

-- | Determina a nova direção de um inimigo com base na posição no mapaDirecional.
--
-- ==== Parâmetros:
-- * MapaDirecional - O mapa que indica as direções.
-- * Inimigo - O inimigo cuja direção será atualizada.
--
-- ==== Resultado:
-- A nova direção do inimigo.
mudarDirecao :: MapaDirecional -> Inimigo -> Direcao
mudarDirecao mapaDir (Inimigo {posicaoInimigo = pos@(posLinha, posColuna), direcaoInimigo = dir})
  | dir == Oeste && desvioLocal posColuna <= 0.5 = naPosicao pos mapaDir
  | dir == Este && desvioLocal posColuna >= 0.5 = naPosicao pos mapaDir
  | dir == Sul && desvioLocal posLinha >= 0.5 = naPosicao pos mapaDir
  | dir == Norte && desvioLocal posLinha <= 0.5 = naPosicao pos mapaDir
  | dir == Repouso = dir
  | otherwise = dir
  where
    -- | Calcula o desvio local da posição atual.
    desvioLocal x = x - fromInteger (floor x)

-- | Calcula a velocidade de um inimigo com efeitos de projéteis aplicados.
--
-- ==== Parâmetros:
-- * [Projetil] - A lista de projéteis que afetam o inimigo.
-- * Float - A velocidade base do inimigo.
--
-- ==== Resultado:
-- A velocidade ajustada do inimigo com base nos efeitos dos projéteis.
velPosProjeteis :: [Projetil] -> Float -> Float
velPosProjeteis projeteis vel
  | Gelo `elem` projeteisAtivos = 0
  | Resina `elem` projeteisAtivos = vel / 2
  | Veneno `elem` projeteisAtivos = vel / (4 / 3)
  | otherwise = vel
  where
    -- | Lista os tipos de projéteis ativos no inimigo.
    projeteisAtivos = map tipoProjetil projeteis
