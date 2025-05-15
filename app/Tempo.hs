module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3

-- | Função que atualiza o estado do jogo com base no tempo.
reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo _ it@(ImmutableTowers {decorrerJogo = MenuPrincipal}) = it
reageTempo _ it@(ImmutableTowers {decorrerJogo = MenuControlos}) = it
reageTempo _ it@(ImmutableTowers {decorrerJogo = MenuNiveis}) = it
reageTempo _ it@(ImmutableTowers {decorrerJogo = Vitoria}) = it
reageTempo _ it@(ImmutableTowers {decorrerJogo = Derrota}) = it
reageTempo _ it@(ImmutableTowers {decorrerJogo = Pausa}) = it
reageTempo tick it@(ImmutableTowers {jogoAtual = jogo, decorrerJogo = Correndo}) =
  it {jogoAtual = atualizaJogo tick jogo, decorrerJogo = comoDecorre}
  where
    comoDecorre
      | perdeuJogo jogo = Derrota
      | ganhouJogo jogo = Vitoria
      | otherwise = Correndo
