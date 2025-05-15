module DesenharJogo where

import Graphics.Gloss
import Imagem
import ImmutableTowers
import LI12425
import Tarefa3

-- Função principal para desenhar o estado do jogo
desenhaJogo :: ImmutableTowers -> Picture
desenhaJogo ImmutableTowers {jogoAtual = jogo, imagens = imagem} =
  Pictures
    [ rectangleSolid 1930 1090,
      color (makeColorI 0 150 200 255) $ rectangleSolid 1920 1080,
      desenhaMapa $ mapaJogo jogo,
      desenhaTorres jogo,
      desenhaInimigos $ inimigosJogo jogo,
      desenhaBase $ baseJogo jogo,
      desenhaPortais $ portaisJogo jogo,
      desenhaLasers $ lasersJogo jogo,
      desenhaLoja
    ]
  where
    -- Funções relacionadas ao desenho das torres
    desenhaTorres :: Jogo -> Picture
    desenhaTorres Jogo {torresJogo = torres} =
      Pictures $ map desenhaTorre torres

    desenhaTorre :: Torre -> Picture
    desenhaTorre
      Torre
        { defenidaTorre = defenida,
          posicaoTorre = pos,
          projetilTorre = Projetil {tipoProjetil = tipo}
        }
        | not defenida =
            uncurry translate (matrizParaCartesiano jogo pos) $
              torreVazia imagem
        | tipo == Resina =
            uncurry translate (matrizParaCartesiano jogo (somaPosicao pos (-0.5, -0.5))) $
              Pictures [torreResina imagem]
        | tipo == Fogo =
            uncurry translate (matrizParaCartesiano jogo (somaPosicao pos (-0.5, -0.5))) $
              Pictures [torreFogo imagem]
        | tipo == Gelo =
            uncurry translate (matrizParaCartesiano jogo (somaPosicao pos (-0.5, -0.5))) $
              Pictures [torreGelo imagem]
        | tipo == Veneno =
            uncurry translate (matrizParaCartesiano jogo (somaPosicao pos (-0.5, -0.5))) $
              Pictures [torreVeneno imagem]
        | otherwise = Pictures []

    -- Funções relacionadas ao desenho da loja
    desenhaLoja :: Picture
    desenhaLoja = Pictures $ map desenhaLojaAux (torresJogo jogo)

    desenhaLojaAux :: Torre -> Picture
    desenhaLojaAux Torre {posicaoTorre = pos, menuAbertoTorre = aberto}
      | aberto = uncurry translate (matrizParaCartesiano jogo (somaPosicao pos (-1.5, -1.5))) (menuLoja imagem)
      | otherwise = blank

    -- Funções relacionadas ao desenho dos portais
    desenhaPortais :: [Portal] -> Picture
    desenhaPortais portais = Pictures $ map desenhaPortal portais
      where
        desenhaPortal Portal {posicaoPortal = pos} =
          uncurry translate (matrizParaCartesiano jogo (somaPosicao pos (-0.5, -0.5))) $
            portal imagem

    -- Funções relacionadas ao desenho dos inimigos
    desenhaInimigos :: [Inimigo] -> Picture
    desenhaInimigos inimigos = Pictures $ map desenhaInimigo inimigos

    desenhaInimigo :: Inimigo -> Picture
    desenhaInimigo Inimigo {posicaoInimigo = pos, vidaInimigo, projeteisInimigo = projeteis, direcaoInimigo = direcao, spritesInimigo = sprites} =
      uncurry translate (matrizParaCartesiano jogo $ somaPosicao pos (-0.5, -0.5)) $
        Pictures
          [ desenhaCorpoInimigo,
            desenhaVidaInimigo,
            indicadorDeProjeteis
          ]
      where
        -- Função que desenha o corpo do inimigo com base na sua direção e no estado atual do sprite.
        desenhaCorpoInimigo = sprite
          where
            -- O primeiro elemento da lista `sprites` representa o estado atual do inimigo.
            spriteAtual = head sprites
            sprite
              -- Seleção do sprite quando o inimigo está parado, baseado na direção.
              | direcao == Este && spriteAtual == Parado = inimigoEsteP imagem
              | direcao == Oeste && spriteAtual == Parado = inimigoOesteP imagem
              | direcao == Norte && spriteAtual == Parado = inimigoNorteP imagem
              | direcao == Sul && spriteAtual == Parado = inimigoSulP imagem
              --
              -- Seleção do sprite quando o inimigo está a andar para a direita.
              | direcao == Este && spriteAtual == AndarD = inimigoEsteD imagem
              | direcao == Oeste && spriteAtual == AndarD = inimigoOesteD imagem
              | direcao == Norte && spriteAtual == AndarD = inimigoNorteD imagem
              | direcao == Sul && spriteAtual == AndarD = inimigoSulD imagem
              --
              -- Seleção do sprite quando o inimigo está a andar para a esquerda.
              | direcao == Este && spriteAtual == AndarE = inimigoEsteE imagem
              | direcao == Oeste && spriteAtual == AndarE = inimigoOesteE imagem
              | direcao == Norte && spriteAtual == AndarE = inimigoNorteE imagem
              | direcao == Sul && spriteAtual == AndarE = inimigoSulE imagem
              --
              -- Sprite padrão caso nenhum dos estados acima seja satisfeito.
              | otherwise = color rose $ rectangleSolid 30 30

        -- Função que desenha a barra de vida do inimigo acima da sua posição.
        desenhaVidaInimigo = translate 0 30 $ color red $ rectangleSolid vidaInimigo 10

        -- Função que desenha o indicador de projéteis do inimigo acima da barra de vida.
        indicadorDeProjeteis = translate 0 45 $ desenhaIndicadorDeProjeteis (map tipoProjetil projeteis)
          where
            -- Função que mapeia um TipoProjetil para a sua cor correspondente.
            corDoProjetil :: TipoProjetil -> Color
            corDoProjetil Fogo = makeColorI 234 79 54 255 -- Vermelho
            corDoProjetil Resina = makeColorI 213 224 75 255 -- Laranja
            corDoProjetil Veneno = makeColorI 160 32 240 255 -- Roxo
            corDoProjetil Gelo = makeColorI 77 155 230 255 -- Azul

            -- Função que gera uma imagem (Picture) representando uma lista de projéteis.
            -- Cada projétil é representado por um quadrado 10x10 da cor correspondente,
            -- dispostos horizontalmente com um espaço entre eles.
            desenhaIndicadorDeProjeteis :: [TipoProjetil] -> Picture
            desenhaIndicadorDeProjeteis tipos = translate (-metadeLargura) 0 $ pictures quadradosPosicionados
              where
                -- Ordena os tipos de projéteis conforme a sequência Fogo, Resina, Veneno, Gelo.
                tiposOrdenados = [Fogo, Resina, Veneno, Gelo]
                -- Cria os quadrados coloridos para os tipos presentes na lista de entrada.
                quadrados = [color (corDoProjetil tipo) $ rectangleSolid 13 13 | tipo <- tiposOrdenados, tipo `elem` tipos]
                -- Calcula as posições para cada quadrado.
                posicoes = [fromIntegral (i * 13) | i <- [0 .. length quadrados - 1]]
                -- Combina cada quadrado com sua posição.
                quadradosPosicionados = (zipWith (`translate` 0) posicoes [color black $ rectangleSolid 15 15 | tipo <- tiposOrdenados, tipo `elem` tipos]) ++ zipWith (`translate` 0) posicoes quadrados
                -- Calcula metade da largura total para centralizar a imagem.
                metadeLargura = fromIntegral (length quadrados * 13) / 2

    -- Função que gera Pictures dos Lasers do Jogo
    desenhaLasers :: [Laser] -> Picture
    desenhaLasers lasers = Pictures $ map desenhaLasersAux lasers
      where
        desenhaLasersAux :: Laser -> Picture
        desenhaLasersAux (Laser {posicaoTorreLaser = pTorre, posicaoInimigoLaser = pInimigo, tipoLaser = tipo, tempoLaser = tempo}) = color (corDoLaser tipo) $ polygon [pT, p1, p2]
          where
            pT@(ptx, pty) = matrizParaCartesiano jogo $ somaPosicao (-1, -1) pTorre
            pI@(pix, piy) = matrizParaCartesiano jogo $ somaPosicao (-0.4, -0.4) pInimigo
            p1 = somaPosicao (-vx, -vy) pI
            p2 = somaPosicao (vx, vy) pI
            (vx, vy) = (-(escalar * ((piy - pty) / dist pT pI)), escalar * (pix - ptx) / dist pT pI)
            escalar = 15 * calculaEscalar jogo
            -- Função que mapeia um TipoProjetil para a sua cor correspondente.
            corDoLaser :: TipoProjetil -> Color
            corDoLaser Fogo = makeColorI 234 79 54 opacidade -- Vermelho
            corDoLaser Resina = makeColorI 213 224 75 opacidade -- Laranja
            corDoLaser Veneno = makeColorI 160 32 240 opacidade -- Roxo
            corDoLaser Gelo = makeColorI 77 155 230 opacidade -- Azul
            opacidade = truncate (tempo * 2 * (255 + 200))

    -- Função relacionada ao desenho da base
    desenhaBase :: Base -> Picture
    desenhaBase Base {posicaoBase = pos, creditosBase = creds, vidaBase = vida} =
      Pictures
        [ barraVida,
          translate (1750 / 2) (900 / 2) $ scale 2 2 $ scale (0.17 / (0.74 * calculaEscalar jogo)) (0.17 / (0.74 * calculaEscalar jogo)) (base imagem),
          uncurry translate (matrizParaCartesiano jogo (somaPosicao pos (-0.5, -0.5))) $ base imagem,
          translate (-(1900 / 2)) (900 / 2) $ scale 2 2 $ desenharCreditos creds
        ]
      where
        barraVida :: Picture
        barraVida =
          translate (-20) 0 $
            Pictures
              [ polygon [(870, 415), (870, 485), (840 - vida * 4, 485), (840 - vida * 4, 415)],
                color green $ polygon [(850, 425), (850, 475), (850 - vida * 4, 475), (850 - vida * 4, 425)]
              ]

        -- Função para converter um dígito na sua Picture correspondente
        digitoParaImagem :: Int -> Picture
        digitoParaImagem 0 = zero imagem
        digitoParaImagem 1 = um imagem
        digitoParaImagem 2 = dois imagem
        digitoParaImagem 3 = tres imagem
        digitoParaImagem 4 = quatro imagem
        digitoParaImagem 5 = cinco imagem
        digitoParaImagem 6 = seis imagem
        digitoParaImagem 7 = sete imagem
        digitoParaImagem 8 = oito imagem
        digitoParaImagem 9 = nove imagem
        digitoParaImagem _ = blank

        -- Função para converter uma string de dígitos em uma lista de Ints
        stringParaDigitos :: String -> [Int]
        stringParaDigitos = map (read . (: []))

        -- Função para criar uma lista de Pictures a partir de um score Int
        pontuacaoParaImagens :: Int -> [Picture]
        pontuacaoParaImagens pontuacao = map digitoParaImagem digitos
          where
            digitos = stringParaDigitos (show pontuacao)

        -- Função para criar a imagem final com dígitos espaçados uniformemente
        desenharCreditos :: Int -> Picture
        desenharCreditos pontuacao = pictures $ translate 35 0 (moeda imagem) : zipWith (uncurry translate) posicoes imagensDigitos
          where
            imagensDigitos
              | length (pontuacaoParaImagens pontuacao) < 4 = replicate (4 - length (pontuacaoParaImagens pontuacao)) (zero imagem) ++ pontuacaoParaImagens pontuacao
              | otherwise = pontuacaoParaImagens pontuacao
            posicoes = zip [x * 45 | x <- [2 ..]] [x * 0 | x <- [0 ..]] -- 40 é a largura de cada dígito + 5 de espaçamento

    -- Funções relacionadas ao desenho do mapa
    desenhaMapa :: Mapa -> Picture
    desenhaMapa mapa = Pictures $ desenhaMapaAux mapa (0.5, 0.5)

    desenhaMapaAux :: Mapa -> Posicao -> [Picture]
    desenhaMapaAux [] _ = []
    desenhaMapaAux ([] : xss) (n, _) = desenhaMapaAux xss (n + 1, 0.5)
    desenhaMapaAux ((x : xs) : xss) (n, m) =
      uncurry translate (matrizParaCartesiano jogo (n, m)) (terrenoParaImagem x) : desenhaMapaAux (xs : xss) (n, m + 1)

    terrenoParaImagem :: Terreno -> Picture
    terrenoParaImagem Vazio = blank
    terrenoParaImagem Relva = relva imagem
    terrenoParaImagem Agua = rio imagem
    terrenoParaImagem Terra = rua imagem
    terrenoParaImagem Areia = areia imagem
    terrenoParaImagem Neve = neve imagem

gridSize :: Jogo -> Float
gridSize jogo = 375 * calculaEscalar jogo

gridBlock :: Jogo -> Picture
gridBlock jogo = rectangleSolid (gridSize jogo) (gridSize jogo)

-- Funções auxiliares
largura :: Mapa -> Float
largura mapa = fromIntegral (length (head mapa)) / 2

altura :: Mapa -> Float
altura mapa = fromIntegral (length mapa) / 4

somaPosicao :: Posicao -> Posicao -> Posicao
somaPosicao (a, b) (c, d) = (a + c, b + d)

-- Funcao que converte uma coordenada matricial representativa do jogo e usada na sua lo'gica do tipo (x,y)
--  sendo x e y ambos >= 0 para uma coordenada Cartesiana representativa da coordenada no ecra, usada para a parte grafica
matrizParaCartesiano :: Jogo -> Posicao -> Posicao
matrizParaCartesiano jogo (n, m) = somaPosicao (recenterX, recenterY) ((-(n * offsetX)) + m * offsetX, (-(n * offsetY)) - m * offsetY)
  where
    offsetX = 375 * calculaEscalar jogo / 2
    offsetY = 109 * calculaEscalar jogo
    recenterX :: Float
    recenterX = (fromIntegral (length $ mapaJogo jogo) - fromIntegral (length $ head $ mapaJogo jogo)) / 4 * (375 * escalar)
    recenterY :: Float
    recenterY = (fromIntegral (length $ mapaJogo jogo) + fromIntegral (length $ head $ mapaJogo jogo)) / 2 * 109 * escalar
    escalar = calculaEscalar jogo

{-
Esta função é intrínseca à representação visual de um mapa no jogo. No gloss, a coordenada
central do ecra é (0,0) e, utilizando esta função, calculamos um valor que permite maximizar o
 tamanho do mapa no ecra, seguindo alguns parâmetros e limitando-se a uma largura de 1300 pixels.
Ou seja, o pixel mais à direita do mapa e o pixel mais à esquerda sempre terão entre eles
 um espaçamento horizontal de 1300 pixels, independentemente do tamanho do mapa.
Desta forma, temos uma representação otimizada de qualquer mapa,
independentemente do seu tamanho e formato.

Este escalonamento é também fundamental para centralizar o mapa no ecra, confinando-o
 a um retângulo imaginário com centro em (0,0), altura infinita e largura de 1300 pixels.
-}
calculaEscalar :: Jogo -> Float
calculaEscalar jogo = 1000 / ((fromIntegral (length $ mapaJogo jogo) + fromIntegral (length $ head $ mapaJogo jogo)) * 940 * cos 30)
