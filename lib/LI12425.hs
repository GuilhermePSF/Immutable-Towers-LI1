-- |
-- Module      : LI12425
-- Description : Definições base do jogo
-- Copyright   : Nelson Estevão <d12733@di.uminho.pt>
--               Olga Pacheco   <omp@di.uminho.pt>
--               Pedro Peixoto  <d14110@di.uminho.pt>
--               Xavier Pinho   <d12736@di.uminho.pt>
--
-- Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2024/25.
module LI12425
  ( -- * Tipos de dados
    Decorrer (..),

    -- ** Básicos
    Creditos,
    Direcao (..),
    Distancia,
    Duracao (..),
    Posicao,
    Semente,
    Tempo,
    SpriteInimigo (..),

    -- ** Mapas
    Mapa,
    Terreno (..),
    MapaDirecional,

    -- ** Entidades
    Base (..),
    Torre (..),
    Portal (..),
    Inimigo (..),
    TipoProjetil (..),
    Projetil (..),

    -- ** Jogo
    Jogo (..),
    Onda (..),
    Laser (..),
    Loja,

    -- * Funções auxiliares
    geraAleatorios,
  )
where

import System.Random (mkStdGen, randoms)

-- | Tipo de terrenno do mapa.
data Terreno
  = -- | Torres constroem-se sobre o relvado do mapa.
    Relva
  | -- | A base e os portais constroem-se sobre caminhos de terra do mapa. Além disso, inimigos movem-se sobre estes terrenos.
    Terra
  | -- | Água para efeito decorativo, mas onde não se pode construir, nem os inimigos se podem mover.
    Agua
  | -- | Torres constroem-se sobre o relvado do mapa (Este Terreno tem o mesmo proposito que o Terreno Relva).
    Areia
  | -- | Torres constroem-se sobre o relvado do mapa (Este Terreno tem o mesmo proposito que o Terreno Relva).
    Neve
  | -- | Vazio para efeito decorativo, mas onde não se pode construir, nem os inimigos se podem mover.
    Vazio
  deriving (Eq, Show, Read)

-- | Mapa do jogo composto por uma matriz de terrenos.
type Mapa = [[Terreno]]

-- | Mapa das direções do caminho.
type MapaDirecional = [[Direcao]]

-- | Coordenada bilateral de uma entidade no jogo, representante do seu centro.
-- O referencial tem origem no canto superior esquerdo, com eixos x e y positivos para a direita e para baixo, respectivamente.
type Posicao = (Float, Float)

-- | Moeda do jogo.
type Creditos = Int

-- | Base de operações do jogador.
data Base = Base
  { -- | Vida da base. Quando esta chega a zero, o jogador perde o jogo.
    vidaBase :: Float,
    -- | Posição da base no mapa. A base deve estar sobre um terreno de terra.
    posicaoBase :: Posicao,
    -- | Balanço de créditos do jogador.
    creditosBase :: Creditos
  }
  deriving (Eq, Show, Read)

-- | Distância entre duas posições.
type Distancia = Float

-- | Tempo em segundos.
type Tempo = Float

-- | Representa uma duração em segundos
data Duracao
  = -- | Duração em segundos
    Finita Tempo
  | -- | Duração infinita
    Infinita
  deriving (Eq, Show, Read, Ord)

-- | Torre que dispara projéteis contra inimigos.
data Torre = Torre
  { -- | Posição da torre no mapa.
    posicaoTorre :: Posicao,
    -- | Redução de vida no inimigo pelo impacto do projétil.
    danoTorre :: Float,
    -- | Alcance circular da torre.
    alcanceTorre :: Float,
    -- | Número de máximo de inimigos simultaneamente atingidos por uma rajada de tiros.
    rajadaTorre :: Int,
    -- | Ciclo de tempo entre rajadas de tiros.
    cicloTorre :: Tempo,
    -- | Tempo restante para a próxima rajada de tiros.
    tempoTorre :: Tempo,
    -- | Efeito secundário associado ao tipo de projétil da torre.
    projetilTorre :: Projetil,
    -- | Booleano que indica se o menu de selecao esta aberto
    menuAbertoTorre :: Bool,
    -- | Diz se a torre ja esta defenida(True) ou se esta vazia(False)
    defenidaTorre :: Bool
  }
  deriving (Eq, Show, Read)

-- | Laser que representa o diparo de um enimigo
data Laser = Laser
    {
      -- | Ponto de inicio de um Laser.
      posicaoTorreLaser :: Posicao,
      -- | Ponto de chegada de um Laser.
      posicaoInimigoLaser :: Posicao,
      -- | Tipo do projetil da torre à qual o laser origina.
      tipoLaser :: TipoProjetil,
      -- | Tempo restante ate o Laser desaparecer.
      tempoLaser :: Tempo
    }
    deriving (Eq, Show, Read)


-- | Loja de torres disponíveis para construir por uma quantidade de créditos.
type Loja = [(Creditos, Torre)]

-- | Tipo de projétil disparado por uma torre.
data TipoProjetil = Fogo | Gelo | Resina | Veneno
  deriving (Eq, Show, Read)

-- | Projétil aplicado por uma torre.
data Projetil = Projetil
  { -- | Tipo de projétil.
    tipoProjetil :: TipoProjetil,
    -- | Duração do efeito do projétil no inimigo.
    duracaoProjetil :: Duracao
  }
  deriving (Eq, Show, Read)

-- | Direção de movimento de uma entidade no jogo.
data Direcao
  = Norte
  | Sul
  | Este
  | Oeste
  | Repouso
  deriving (Eq, Show, Read)

-- | Inimigo que se move em direção à base do jogador.
data Inimigo = Inimigo
  { -- | Posição do inimigo no mapa.
    posicaoInimigo :: Posicao,
    -- | Direção do último movimento do inimigo.
    direcaoInimigo :: Direcao,
    -- | Vida do inimigo.
    vidaInimigo :: Float,
    -- | Velocidade do inimigo.
    velocidadeInimigo :: Float,
    -- | Dano causado pelo inimigo na base do jogador.
    ataqueInimigo :: Float,
    -- | Créditos que o jogador recebe ao derrotar o inimigo.
    butimInimigo :: Creditos,
    -- | Efeitos secundários ativos no inimigo.
    projeteisInimigo :: [Projetil],
    -- | Sprites do Inimigo.
    spritesInimigo :: [SpriteInimigo],
    -- | Distancia percorrida entre cada trocar de sprite.
    spriteCiclo :: Distancia,
    -- | Distancia restante ate trocar de sprite.
    spriteDistancia :: Distancia
  }
  deriving (Eq, Show, Read)

-- | Sprite do inimigo.
data SpriteInimigo = AndarE | Parado | AndarD
  deriving (Eq, Show, Read)

-- | Onda de inimigos que saem de um portal.
data Onda = Onda
  { -- | Inimigos que compõem a onda.
    inimigosOnda :: [Inimigo],
    -- | Tempo em segundos entre a entrada de cada inimigo.
    cicloOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada do próximo inimigo da onda.
    tempoOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada da onda.
    entradaOnda :: Tempo
  }
  deriving (Eq, Show, Read)

-- | Portal de entrada de inimigos no mapa.
data Portal = Portal
  { -- | Posição do portal no mapa. O portal deve estar sobre um terreno de terra.
    posicaoPortal :: Posicao,
    -- | Ondas de inimigos que saem do portal.
    ondasPortal :: [Onda]
  }
  deriving (Eq, Show, Read)

-- | Estado do jogo. Determina o que está a decorrer no jogo.
data Decorrer
  = Vitoria
  | Derrota
  | Pausa
  | Correndo
  | MenuPrincipal
  | MenuControlos
  | MenuNiveis
  deriving (Eq, Show, Read)

-- | Estado do jogo. Um jogo é composto pela base, vários portais, várias torres, um mapa, vários inimigos e a loja.
data Jogo = Jogo
  { -- | Base de operações do jogador.
    baseJogo :: Base,
    -- | Portais de entrada de inimigos no mapa.
    portaisJogo :: [Portal],
    -- | Torres construídas pelo jogador.
    torresJogo :: [Torre],
    -- | Mapa retangular do jogo.
    mapaJogo :: Mapa,
    -- | Mapa direcional retangular do jogo.
    mapaDirecaoJogo :: MapaDirecional,
    -- | Inimigos em movimento no mapa.
    inimigosJogo :: [Inimigo],
    -- | Lasers visiveis no ecra.
    lasersJogo :: [Laser],
    -- | Loja de torres disponíveis para construir.
    lojaJogo :: Loja
  }
  deriving (Eq, Show, Read)

-- | Valor inicial que determina a sequência de números pseudo-aleatórios.
type Semente = Int

-- | Função que gera uma lista de números aleatórios a partir de uma 'Semente'.
--
-- == Exemplos
--
-- >>> geraAleatorios 2425 3
-- [9108974057934916489,3509742222561512871,1534041518507426227]
--
-- >>> geraAleatorios 10 1
-- [3575835729477015470]
geraAleatorios :: Semente -> Int -> [Int]
geraAleatorios s c = take c $ randoms (mkStdGen s)
