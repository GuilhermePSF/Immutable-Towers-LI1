module Desenhar where

import DesenharJogo
import Graphics.Gloss
import Imagem
import ImmutableTowers
import LI12425

-- Função que desenha o estado atual do jogo.
desenha :: ImmutableTowers -> Picture
-- Mostra o jogo atual
desenha it@(ImmutableTowers {decorrerJogo = Correndo}) = desenhaJogo it
-- Mostra o jogo atual com um overlay do menu de Pausa e as suas opcoes
desenha it@(ImmutableTowers {decorrerJogo = Pausa, imagens = imagem}) = Pictures [desenhaJogo it, menuPausa imagem]
desenha ImmutableTowers {decorrerJogo = MenuPrincipal, imagens = imagem} = menuPrincipal imagem
desenha ImmutableTowers {decorrerJogo = MenuNiveis, imagens = imagem} = menuNiveis imagem
desenha ImmutableTowers {decorrerJogo = MenuControlos, imagens = imagem} = menuControlos imagem
-- Mostra o menu de vitoria e as suas opcoes
desenha ImmutableTowers {decorrerJogo = Vitoria, imagens = imagem} = menuVitoria imagem
-- Mostra o menu de derrota e as suas opcoes
desenha ImmutableTowers {decorrerJogo = Derrota, imagens = imagem} = menuDerrota imagem
