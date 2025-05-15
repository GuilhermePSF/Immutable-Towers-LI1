module ImmutableTowers where

import Imagem
import LI12425

-- Tipo de dados que representa o estado do jogo.
--   Contém informações sobre o jogo atual, o jogo inicial, o decorrer do jogo e as imagens associadas.
data ImmutableTowers = ImmutableTowers
  { -- O estado atual do jogo.
    -- Este campo armazena a configuração atual do jogo, como a posição das peças e o estado das torres.
    jogoAtual :: Jogo,
    -- O estado inicial do jogo.
    -- Este campo mantém uma cópia do jogo inicial, sendo este o jogo foi carregado, que pode ser usado para restaurar o jogo ao seu estado original.
    jogoInicial :: Jogo,
    -- O progresso do jogo.
    -- Este campo indica o andamento do jogo, podendo ser, por exemplo, se o jogo está em curso, pausado, se ja venceu ou perdeu ou se esta num certo menu, como por exemplo o menu principal.
    decorrerJogo :: Decorrer,
    -- As imagens associadas ao jogo.
    -- Este campo armazena as imagens usadas para ilustrar os diferentes elementos do jogo.
    imagens :: Imagens
  }
