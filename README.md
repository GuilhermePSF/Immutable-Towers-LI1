<h1 style="display: inline-flex; align-items: center;">
  
  <div style="font-size: 90px; margin-bottom: 20px line-height: 1;">
    <img src="https://raw.githubusercontent.com/GuilhermePSF/Immutable-Towers-LI1/refs/heads/main/images/inimigo/sulD.bmp" 
       alt="Icon" 
       style="height: 60px; margin-top: 10px;" />
    I M M U T A B L E&nbsp;&nbsp;T O W E R S
  <img src="https://raw.githubusercontent.com/GuilhermePSF/Immutable-Towers-LI1/refs/heads/main/images/inimigo/esteD.bmp" 
       alt="Icon" 
       style="height: 60px; margin-top: 10px;" />
  </div>
</h1>

## Visão Geral do Projeto

Este projeto foi desenvolvido no âmbito da unidade curricular **Laboratórios de Informática I** (2024/2025), da **Licenciatura em Engenharia Informática** da **Universidade do Minho**.

**Immutable Towers** é uma interpretação funcional do clássico género *Tower Defense*, desenvolvida em **Haskell**, com interface gráfica baseada em **gloss**. O jogador deve posicionar torres estrategicamente para impedir que ondas de inimigos cheguem à base, utilizando diferentes tipos de projéteis com sinergias e efeitos.

> Para mais detalhes sobre as regras do projeto: [Descrição Oficial](https://github.com/GuilhermePSF/Immutable-Towers-LI1/blob/main/Enunciado.pdf)

## Autores

- **Guilherme Ferreira** — A11042  
- **Lucas Pinto** — A111042

## Funcionalidades

- Simulação completa de um jogo *Tower Defense*
- Diferentes tipos de terrenos: relva, terra e água
- Torres com projéteis variados e sinergias:
  - **Fogo** 🔥 – dano contínuo
  - **Gelo** ❄️ – congela inimigos
  - **Resina** 🟡 – reduz velocidade
  - **Veneno** ☠️ – aplica efeitos mais fracos de Fogo e Resina
- Inimigos com estados ativos, direção, vida e velocidade
- Portais com ondas programadas de inimigos
- Sistema de loja e compra de torres
- Interface gráfica com animações, interação por rato e visão isométrica
- Sistema de níveis, saves e carregamento de jogo
- Efeitos visuais e suporte a torres com **lasers**

## Instalação e Execução

Certifique-se de ter o **GHC** e o **Cabal** instalados.

Para compilar e executar o jogo:

```sh
cabal run
```

Para correr os testes:

```sh
cabal test
```

## Estrutura do Projeto

| Módulo       | Descrição                                      |
|--------------|------------------------------------------------|
| `Mapa.hs`    | Representação e validação do mapa              |
| `Base.hs`    | Estado e mecânica da base do jogador           |
| `Torre.hs`   | Definições de torres e projéteis               |
| `Inimigo.hs` | Movimento e efeitos dos inimigos               |
| `Portal.hs`  | Geração e gestão de ondas de inimigos          |
| `Jogo.hs`    | Estado global do jogo                          |
| `Main.hs`    | Interface gráfica usando gloss                 |
| `Testes.hs`  | Testes unitários com **HUnit**                 |

## Extras Implementados

Este projeto inclui diversas funcionalidades adicionais que vão além do enunciado original:

- 🌐 **Isometria**: representação visual mais rica do mapa
- ⚡ **Novos poderes e tipos de torre**, incluindo **lasers**
- 🎯 **Interação por rato**: clique para comprar e posicionar torres
- ✨ **Animações visuais** para eventos no jogo
- 🎮 **Sistema de níveis** com progressão
- 💾 **Sistema de saves** e carregamento de estado do jogo

## Testes

Os testes foram implementados utilizando a biblioteca **HUnit**.

Para executar todos os testes:

```sh
cabal test
```

## Qualidade do Código

- Estrutura modular em Haskell, com separação clara por responsabilidades
- Testes automatizados com HUnit
- Interface reativa com gloss
- Utilização de `git` para controlo de versões
- Código documentado com foco em clareza e manutenção
