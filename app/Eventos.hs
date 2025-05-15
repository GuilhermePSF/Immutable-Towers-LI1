module Eventos (reageEventosIO, aClicar) where

import Data.Char (digitToInt)
import DesenharJogo
import GameStateIO
import Graphics.Gloss.Interface.Pure.Game
import Imagem
import ImmutableTowers
import LI12425
import Tarefa1

-- Função que reage a eventos do jogo.
-- Recebe um evento, o estado atual do jogo tendo em conta o decorrerJogo, e retorna o estado modificado.
reageEventosIO :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosIO e it@ImmutableTowers {decorrerJogo = Correndo} = return $ reageEventosCorrendo e it
reageEventosIO e it@ImmutableTowers {decorrerJogo = Pausa} = reageEventosPausaIO e it
reageEventosIO e it@ImmutableTowers {decorrerJogo = Derrota} = return $ reageEventosDerrota e it
reageEventosIO e it@ImmutableTowers {decorrerJogo = Vitoria} = return $ reageEventosVitoria e it
reageEventosIO e it@ImmutableTowers {decorrerJogo = MenuPrincipal} = reageEventosMenuPrincipal e it
reageEventosIO e it@ImmutableTowers {decorrerJogo = MenuNiveis} = reageEventosMenuNiveis e it
reageEventosIO e it@ImmutableTowers {decorrerJogo = MenuControlos} = return $ reageEventosMenuControlos e it

-- Função que reage a eventos quando o jogo está em pausa.
-- Permite continuar o jogo, guardar o estado atual ou retornar ao menu principal.
reageEventosPausaIO :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosPausaIO (EventKey (SpecialKey KeyEsc) Down _ _) it@(ImmutableTowers {decorrerJogo = Pausa}) = return it {decorrerJogo = Correndo}
reageEventosPausaIO (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) it@(ImmutableTowers {jogoAtual = jogo, jogoInicial = jogoCarregado})
  | aClicar mouseX mouseY 440 45 (0, -95) = return it {decorrerJogo = MenuPrincipal} -- Menu principal
  | aClicar mouseX mouseY 170 45 (0, -180) = error "" -- Sair
  | aClicar mouseX mouseY 200 45 (0, 95) = do
      -- Guardar (Save)
      -- Guardar o estado atual do jogo num ficheiro.
      -- Grava o estado atual do jogo num ficheiro, permitindo retomar o jogo posteriormente.
      saveGameState "app/jogos/gameState.it" jogo
      putStrLn "jogo gravado"
      return it
  | aClicar mouseX mouseY 250 45 (0, 0) = return it {jogoAtual = jogoCarregado, decorrerJogo = Correndo} -- Recomeçar (Retry)
  | otherwise = return it
reageEventosPausaIO _ it = return it

-- Função que reage a eventos no menu principal.
-- Permite navegar entre diferentes opções do menu, como continuar o jogo, ver controlos ou jogar.
reageEventosMenuPrincipal :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosMenuPrincipal (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) it
  | aClicar mouseX mouseY 170 45 (0, -280) = error "" -- Sair
  | aClicar mouseX mouseY 400 45 (0, -185) = return it {decorrerJogo = MenuControlos} -- Controlos
  | aClicar mouseX mouseY 270 45 (0, -95) = do
      -- Continuar (Continue)
      -- Continuar o jogo a partir de um estado gravado num ficheiro.
      -- Tenta carregar um jogo gravado previamente num ficherio. Se bem sucedido, atualiza o estado do jogo.
      putStrLn "carregar jogo"
      maybeJogo <- loadGameState "app/jogos/gameState.it"
      case maybeJogo of
        Just jogo -> do
          let escalar = calculaEscalar jogo
          imagens <- carregarImagem escalar
          if validaJogo jogo then putStrLn "jogo Valido carregado" else putStrLn "jogo Invalido carregado"
          return it {jogoAtual = jogo, jogoInicial = jogo, decorrerJogo = Correndo, imagens = imagens}
        Nothing -> do
          putStrLn "nao carregou direito"
          return it
  | aClicar mouseX mouseY 200 45 (0, 0) = return it {decorrerJogo = MenuNiveis} -- Jogar (Play)
  | otherwise = return it
reageEventosMenuPrincipal _ it = return it

-- Função que reage a eventos no menu de níveis.
-- Permite selecionar diferentes níveis ou retornar ao menu principal.
reageEventosMenuNiveis :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventosMenuNiveis (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) it
  | aClicar mouseX mouseY 150 150 (-560, 0) = do
      -- Carregar o nível 1.
      putStrLn "carregar jogo 1"
      maybeJogo <- loadGameState "app/jogos/nivel01.it"
      case maybeJogo of
        Just jogo -> do
          let escalar = calculaEscalar jogo
          imagens <- carregarImagem escalar
          if validaJogo jogo then putStrLn "jogo Valido carregado" else putStrLn "jogo Invalido carregado"
          return it {jogoAtual = jogo, jogoInicial = jogo, decorrerJogo = Correndo, imagens = imagens}
        Nothing -> do
          putStrLn "nao carregou direito"
          return it
  | aClicar mouseX mouseY 150 150 (0, 0) = do
      -- Carregar o nível 2.
      putStrLn "carregar jogo 2"
      maybeJogo <- loadGameState "app/jogos/nivel02.it"
      case maybeJogo of
        Just jogo -> do
          let escalar = calculaEscalar jogo
          imagens <- carregarImagem escalar
          if validaJogo jogo then putStrLn "jogo Valido carregado" else putStrLn "jogo Invalido carregado"
          return it {jogoAtual = jogo, jogoInicial = jogo, decorrerJogo = Correndo, imagens = imagens}
        Nothing -> do
          putStrLn "nao carregou direito"
          return it
  | aClicar mouseX mouseY 150 150 (560, 0) = do
      -- Carregar o nível 3.
      putStrLn "carregar jogo 3"
      maybeJogo <- loadGameState "app/jogos/nivel03.it"
      case maybeJogo of
        Just jogo -> do
          let escalar = calculaEscalar jogo
          imagens <- carregarImagem escalar
          if validaJogo jogo then putStrLn "jogo Valido carregado" else putStrLn "jogo Invalido carregado"
          return it {jogoAtual = jogo, jogoInicial = jogo, decorrerJogo = Correndo, imagens = imagens}
        Nothing -> do
          putStrLn "nao carregou direito"
          return it
  | aClicar mouseX mouseY 545 60 (10, -460) = return it {decorrerJogo = MenuPrincipal} -- Menu principal
  | otherwise = return it
reageEventosMenuNiveis _ it = return it

-- Função que reage a eventos no menu de controlos.
-- Permite retornar ao menu principal.
reageEventosMenuControlos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosMenuControlos (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) it
  | aClicar mouseX mouseY 545 60 (10, -460) = it {decorrerJogo = MenuPrincipal} -- Menu principal
  | otherwise = it
reageEventosMenuControlos _ it = it

-- Função que reage a eventos quando o jogador vence.
-- Permite retornar ao menu principal ou sair do jogo.
reageEventosVitoria :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosVitoria (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) it
  | aClicar mouseX mouseY 440 45 (0, -95) = it {decorrerJogo = MenuPrincipal} -- Menu principal
  | aClicar mouseX mouseY 170 45 (0, 0) = error "" -- Sair
  | otherwise = it
reageEventosVitoria _ it = it

-- Função que reage a eventos quando o jogador perde.
-- Permite retornar ao menu principal, sair do jogo, ou recomeçar o nível.
reageEventosDerrota :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosDerrota (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) it@(ImmutableTowers {jogoInicial = jogoCarregado})
  | aClicar mouseX mouseY 440 45 (0, -95) = it {decorrerJogo = MenuPrincipal} -- Menu principal
  | aClicar mouseX mouseY 170 45 (0, -0) = error "" -- Sair
  | aClicar mouseX mouseY 250 45 (0, 95) = it {jogoAtual = jogoCarregado, decorrerJogo = Correndo} -- Recomeçar (Retry)
  | otherwise = it
reageEventosDerrota _ it = it

-- Função que reage a eventos quando o jogo está a decorrer.
-- Inclui opções para pausar o jogo, interagir com torres, ou comprar novas torres.
reageEventosCorrendo :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosCorrendo (EventKey (SpecialKey KeyEsc) Down _ _) it@(ImmutableTowers {decorrerJogo = Correndo}) = it {decorrerJogo = Pausa}
reageEventosCorrendo e@(EventKey (Char '1') Down _ _) it@(ImmutableTowers {jogoAtual = jogoAtual}) = it {jogoAtual = reageComprarTorre e jogoAtual}
reageEventosCorrendo e@(EventKey (Char '2') Down _ _) it@(ImmutableTowers {jogoAtual = jogoAtual}) = it {jogoAtual = reageComprarTorre e jogoAtual}
reageEventosCorrendo e@(EventKey (Char '3') Down _ _) it@(ImmutableTowers {jogoAtual = jogoAtual}) = it {jogoAtual = reageComprarTorre e jogoAtual}
reageEventosCorrendo e@(EventKey (Char '4') Down _ _) it@(ImmutableTowers {jogoAtual = jogoAtual}) = it {jogoAtual = reageComprarTorre e jogoAtual}
reageEventosCorrendo (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) it@(ImmutableTowers {jogoAtual = jogo@(Jogo {torresJogo = torres})}) =
  it {jogoAtual = jogo {torresJogo = menusTorres mouseX mouseY}}
  where
    -- Função que abre e fecha o menu das torres.
    menusTorres :: Float -> Float -> [Torre]
    menusTorres ratoX ratoY
      | any menuAbertoTorre torres = map fecharMenu torres
      | otherwise = map abrirMenu torres
      where
        abrirMenu :: Torre -> Torre
        abrirMenu torre@(Torre {posicaoTorre = posTorre})
          | aClicar ratoX ratoY (gridSize jogo) (gridSize jogo / 2) (matrizParaCartesiano jogo (somaPosicao (-0.3, -0.3) posTorre)) = torre {menuAbertoTorre = True}
          | otherwise = torre
        fecharMenu :: Torre -> Torre
        fecharMenu torre@(Torre {posicaoTorre = posTorre})
          | aClicar ratoX ratoY (1920 / 2) (1920 / 2) (matrizParaCartesiano jogo posTorre) = torre {menuAbertoTorre = False}
          | otherwise = torre
reageEventosCorrendo _ it = it

-- Função que permite comprar uma torre.
-- Reage a eventos de teclas associadas a diferentes torres e atualiza o estado do jogo conforme a compra.
reageComprarTorre :: Event -> Jogo -> Jogo
reageComprarTorre (EventKey (Char c) Down _ _) jogo@(Jogo {baseJogo = base@(Base {creditosBase = creditos}), torresJogo = torres, lojaJogo = loja})
  | not (null torresADefinir) && fst (loja !! torreIndice) <= creditos = atualiza torreIndice creditos jogo
  | otherwise = jogo
  where
    torreIndice = digitToInt c - 1

    atualiza idx creditos' j = j {baseJogo = base {creditosBase = creditos' - fst (loja !! idx)}, torresJogo = torresRestantes ++ map (alternarTorre (snd (loja !! idx))) torresADefinir}

    alternarTorre torreaTrocar torreAtual = torreaTrocar {posicaoTorre = posicaoTorre torreAtual}

    torresADefinir = filter (\torre -> menuAbertoTorre torre && not (defenidaTorre torre)) torres

    torresRestantes = filter (not . menuAbertoTorre) torres
reageComprarTorre _ jogo = jogo

-- Função que verifica se um clique está dentro de uma área específica.
-- Recebe as coordenadas do clique e as dimensões da área da hitbox e retorna um booleano.
aClicar :: Float -> Float -> Float -> Float -> Posicao -> Bool
aClicar ratoX ratoY ladoX ladoY (posX, posY) =
  ratoX < limiteXSup && ratoX > limiteXInf && ratoY > limiteYInf && ratoY < limiteYSup
  where
    limiteXSup = posX + ladoX / 2
    limiteYSup = posY + ladoY / 2
    limiteXInf = posX - ladoX / 2
    limiteYInf = posY - ladoY / 2