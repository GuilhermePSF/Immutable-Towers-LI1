-- |
-- Module      : Tarefa2
-- Description : Auxiliares do Jogo
-- Copyright   : Lucas Faria Pinto <a111442@alunos.uminho.pt>
--               Guilherme Pinto da Silva Ferreira <a111042@alunos.uminho.pt>
--
--
-- Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
module Tarefa2 where

import LI12425

-- | Filtra os inimigos que estão ao alcance de uma torre.
-- Um inimigo está ao alcance se a distância entre ele e a torre for menor ou igual ao alcance da torre.
--
-- ==== Parâmetros:
-- * Torre - Torre que está verificando os inimigos.
-- * [Inimigo] - Lista de inimigos.
--
-- ==== Resultado:
-- Lista de inimigos que estão ao alcance.
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance (Torre {posicaoTorre = posTorre, alcanceTorre = alcance}) =
  filter (\inimigo -> dist (posicaoInimigo inimigo) posTorre <= alcance)
  where
    dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

-- | Subtrai à vida do Inimigo o dano da Torre e altera os projeteis relativamente às sinergias entre os projeteis do inimigo e da torre.
-- | Subtrai à vida do Inimigo o dano da Torre e altera os projeteis relativamente às sinergias entre os projeteis do inimigo e da torre.
atingeInimigo :: Torre -> Inimigo -> Inimigo -- Funcao escalavel, esta preparada para mais casos do que os dados no enunciado
atingeInimigo (Torre {projetilTorre = projetil, danoTorre = dano}) inimigo@(Inimigo {vidaInimigo = vida, projeteisInimigo = projeteis}) =
  inimigo {vidaInimigo = vida - dano, projeteisInimigo = sinergias projetil projeteis}

-- | Representa as sinergias entre projéteis de diferentes tipos e durações.
--
--   Recebe um projétil inicial e uma lista de projéteis ativos.
--   Retorna uma lista atualizada de projéteis considerando suas interações.
--
--   === Tipos de Projéteis:
--   * 'Fogo' Pode interagir com projéteis do tipo 'Gelo' e 'Resina'.
--   * 'Gelo' Pode interagir com projéteis do tipo 'Fogo'.
--   * 'Resina' Pode interagir com projéteis do tipo 'Fogo'.
--
--   === Regras:
--   (1) Fogo e Gelo cancelam-se mutuamente, i.e. ambos projéteis devem ser removidos da lista de projéteis do inimigo.
--   (2) Fogo e Resina dobra a duração do fogo, i.e. à lista de projéteis do inimigo deve somente dobrar a duração do projétil Fogo e, caso necessário, remover o projétil Resina da lista.
--   (3) Projéteis iguais somam as suas durações. Por exemplo, Fogo e Fogo, simplesmente resulta num único Fogo com as durações somadas. O mesmopara Gelo e Gelo e Resina e Resina.
--   (4) As restantes combinações de projéteis não resultam em sinergias e, portanto, a lista de projéteis apenas deve ser atualizada com a adição do projétil que atingiu o inimigo.
sinergias :: Projetil -> [Projetil] -> [Projetil]
sinergias projetil@(Projetil Fogo (Finita tempo)) projeteis
  | Gelo `elem` projeteisAtivos =
      filter (\proj -> tipoProjetil proj /= Gelo) projeteis
  | Resina `elem` projeteisAtivos =
      Projetil Fogo (Finita (2 * tempo))
        : filter (\proj -> tipoProjetil proj /= Resina) projeteis
  | otherwise = adicionaTempoDefault projetil projeteis
  where
    projeteisAtivos = map tipoProjetil projeteis
sinergias projetil@(Projetil Gelo _) projeteis
  | Fogo `elem` projeteisAtivos =
      filter (\proj -> tipoProjetil proj /= Fogo) projeteis
  | otherwise = adicionaTempoDefault projetil projeteis
  where
    projeteisAtivos = map tipoProjetil projeteis
sinergias projetil@(Projetil Resina _) projeteis
  | Fogo `elem` projeteisAtivos =
      map
        ( \proj -> case proj of
            Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita t} -> Projetil Fogo (Finita (2 * t))
            _ -> proj
        )
        projeteis
  | otherwise = adicionaTempoDefault projetil projeteis
  where
    projeteisAtivos = map tipoProjetil projeteis
sinergias projetil@(Projetil Veneno _) projeteis = adicionaTempoDefault projetil projeteis
sinergias _ projeteis = projeteis

-- | Adiciona um tempo padrão aos projéteis restantes, garantindo a consistência temporal.
-- | Adiciona um tempo padrão aos projéteis restantes, garantindo a consistência temporal.
-- Caso um projétil com o mesmo tipo já exista, soma o tempo das suas durações.
--
-- ==== Parâmetros:
-- * Projetil - O projétil a ser adicionado ou atualizado.
-- * [Projetil] - A lista de projéteis existentes.
--
-- ==== Resultado:
-- Lista de projéteis atualizada com o tempo adicionado ao projétil correspondente.
adicionaTempoDefault :: Projetil -> [Projetil] -> [Projetil]
adicionaTempoDefault projetil [] = [projetil]
adicionaTempoDefault projetil@(Projetil {duracaoProjetil = Finita t}) (proj@(Projetil tipo (Finita tempo)) : projs)
  | tipoProjetil projetil == tipo = Projetil tipo (Finita (tempo + t)) : projs
  | otherwise = proj : adicionaTempoDefault projetil projs
adicionaTempoDefault projetil@(Projetil {duracaoProjetil = Infinita}) (proj@(Projetil tipo Infinita) : projs)
  | tipoProjetil projetil == tipo = proj : projs
  | otherwise = proj : adicionaTempoDefault projetil projs
adicionaTempoDefault projetil (proj : projs) = proj : adicionaTempoDefault projetil projs

-- | Ativa o próximo inimigo de um portal, adicionando-o à lista de inimigos ativos.
-- Se não houver mais ondas ou inimigos na onda atual, o portal é atualizado apropriadamente.
--
-- ==== Parâmetros:
-- * Portal - O portal de onde o inimigo será ativado.
-- * [Inimigo] - A lista de inimigos ativos.
--
-- ==== Resultado:
-- Uma tupla contendo o portal atualizado e a lista de inimigos ativos atualizada.
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigosAtivos
  | null (ondasPortal portal) = (portal, inimigosAtivos)
  | null (inimigosOnda (head (ondasPortal portal))) = (portal {ondasPortal = tail (ondasPortal portal)}, inimigosAtivos)
  | otherwise =
      ( portal
          { ondasPortal =
              (head (ondasPortal portal))
                { inimigosOnda =
                    tail
                      (inimigosOnda (head (ondasPortal portal)))
                }
                : tail (ondasPortal portal)
          },
        inimigosAtivos ++ [head (inimigosOnda (head (ondasPortal portal)))]
      )

-- | Verifica se o jogo terminou.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se o jogo terminou; caso contrário, retorna 'False'.
--
-- ==== Resultado:
-- 'True' se o jogador ganhou ou perdeu o jogo; caso contrário, 'False'.
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

-- | Verifica se o jogador ganhou o jogo.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se o jogador ganhou o jogo; caso contrário, retorna 'False'.
--
-- ==== Critérios de Vitória:
-- 1. Não deve haver inimigos no jogo.
-- 2. Todas as ondas de todos os portais devem estar vazias.
-- 3. A vida da base deve ser maior que zero.
--
-- ==== Resultado:
-- 'True' se o jogador ganhou o jogo; caso contrário, 'False'.
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) && all (all (null . inimigosOnda) . ondasPortal) (portaisJogo jogo) && vidaBase (baseJogo jogo) > 0

-- | Verifica se o jogador perdeu o jogo.
-- | Verifica se o jogador perdeu o jogo.
-- Recebe um objeto do tipo 'Jogo' e retorna 'True' se o jogador perdeu o jogo; caso contrário, retorna 'False'.
--
-- ==== Critérios de Derrota:
-- 1. A vida da base deve ser menor ou igual a zero.
--
-- ==== Resultado:
-- 'True' se o jogador perdeu o jogo; caso contrário, 'False'.
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0
