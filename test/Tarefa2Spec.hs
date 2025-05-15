module Tarefa2Spec (testesTarefa2, jogo) where

import LI12425
import Tarefa2
import Test.HUnit

p :: Portal
p =
  Portal
    { posicaoPortal = (5.5, 2.5),
      ondasPortal =
        [ Onda
            { inimigosOnda =
                [ Inimigo
                    { posicaoInimigo = (5.2, 3.4),
                      direcaoInimigo = Sul,
                      vidaInimigo = 80,
                      velocidadeInimigo = 1.0,
                      ataqueInimigo = 15,
                      butimInimigo = 10,
                      projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                      spritesInimigo = [Parado, AndarE, Parado, AndarD],
                      spriteCiclo = 0.3,
                      spriteDistancia = 0.3
                    }
                ],
              cicloOnda = 1.5,
              tempoOnda = 0.5,
              entradaOnda = 3
            }
        ]
    }

i :: [Inimigo]
i =
  [ Inimigo
      { posicaoInimigo = (5.2, 3.4),
        direcaoInimigo = Sul,
        vidaInimigo = 80,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 15,
        butimInimigo = 10,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
        spritesInimigo = [Parado, AndarE, Parado, AndarD],
        spriteCiclo = 0.3,
        spriteDistancia = 0.3
      }
  ]

resultadoativaInimigo :: (Portal, [Inimigo])
resultadoativaInimigo =
  ( Portal
      { posicaoPortal = (5.5, 2.5),
        ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 1.5, tempoOnda = 0.5, entradaOnda = 3.0}]
      },
    [ Inimigo
        { posicaoInimigo = (5.2, 3.4),
          direcaoInimigo = Sul,
          vidaInimigo = 80.0,
          velocidadeInimigo = 1.0,
          ataqueInimigo = 15.0,
          butimInimigo = 10,
          projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
          spritesInimigo = [Parado, AndarE, Parado, AndarD],
          spriteCiclo = 0.3,
          spriteDistancia = 0.3

        },
      Inimigo
        { posicaoInimigo = (5.2, 3.4),
          direcaoInimigo = Sul,
          vidaInimigo = 80.0,
          velocidadeInimigo = 1.0,
          ataqueInimigo = 15.0,
          butimInimigo = 10,
          projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
          spritesInimigo = [Parado, AndarE, Parado, AndarD],
          spriteCiclo = 0.3,
          spriteDistancia = 0.3
        }
    ]
  )

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ "ativa inimigo" ~: resultadoativaInimigo ~=? ativaInimigo p i,
        "terminou jogo" ~: False ~=? terminouJogo jogo,
        "ganhou jogo" ~: False ~=? ganhouJogo jogo,
        "perdeu jogo" ~: False ~=? perdeuJogo jogo,
        testsInimigosNoAlcance,
        testsAtingeInimigo,
        testsSinergias
      ]

testsInimigosNoAlcance :: Test
testsInimigosNoAlcance =
  TestList
    [ "Inimigo no alcance"
        ~: [Inimigo (1.5, 0.5) Oeste 50 0.5 5 2 [Projetil Fogo (Finita 4.0)] [Parado, AndarE, Parado, AndarD] 0.3 0.3]
        ~=? inimigosNoAlcance
          (Torre (1.0, 0.0) 20 2.0 1 1 0.0 (Projetil Gelo (Finita 2.0)) False True)
          [Inimigo (1.5, 0.5) Oeste 50 0.5 5 2 [Projetil Fogo (Finita 4.0)] [Parado, AndarE, Parado, AndarD] 0.3 0.3],
      "Nenhum inimigo no alcance"
        ~: []
        ~=? inimigosNoAlcance
          (Torre (0.0, 0.0) 20 1.0 1 1 0.0 (Projetil Gelo (Finita 2.0)) False True)
          [Inimigo (1.5, 0.5) Oeste 50 0.5 5 2 [Projetil Fogo (Finita 4.0)] [Parado, AndarE, Parado, AndarD] 0.3 0.3]
    ]

testsAtingeInimigo :: Test
testsAtingeInimigo =
  TestList
    [ "Aplica dano e projetil"
        ~: Inimigo (1.5, 0.5) Oeste 30 0.5 5 2 [Projetil Gelo (Finita 2.0)] [Parado, AndarE, Parado, AndarD] 0.3 0.3
        ~=? atingeInimigo
          (Torre (1.0, 0.0) 20 2.0 1 1 0.0 (Projetil Gelo (Finita 2.0)) False True)
          (Inimigo (1.5, 0.5) Oeste 50 0.5 5 2 [] [Parado, AndarE, Parado, AndarD] 0.3 0.3),
      "Aplica sinergia de projéteis"
        ~: Inimigo (1.5, 0.5) Oeste 30 0.5 5 2 [Projetil Fogo (Finita 8.0)] [Parado, AndarE, Parado, AndarD] 0.3 0.3
        ~=? atingeInimigo
          (Torre (1.0, 0.0) 20 2.0 1 1 0.0 (Projetil Fogo (Finita 4.0)) False True)
          (Inimigo (1.5, 0.5) Oeste 50 0.5 5 2 [Projetil Resina Infinita] [Parado, AndarE, Parado, AndarD] 0.3 0.3)
    ]

testsSinergias :: Test
testsSinergias =
  TestList
    [ "Fogo -> ()" ~: [fogo] ~=? sinergias fogo [],
      "Fogo -> (Fogo)" ~: [fogo {duracaoProjetil = Finita 8.0}] ~=? sinergias fogo [fogo],
      "Fogo -> (Gelo)" ~: [] ~=? sinergias fogo [gelo],
      "Fogo -> (Resina)" ~: [fogo {duracaoProjetil = Finita 8.0}] ~=? sinergias fogo [resina],
      "Fogo -> (Gelo, Resina)" ~: [resina] ~=? sinergias fogo [gelo, resina],
      --
      "Gelo -> ()" ~: [gelo] ~=? sinergias gelo [],
      "Gelo -> (Fogo)" ~: [] ~=? sinergias gelo [fogo],
      "Gelo -> (Gelo)" ~: [gelo {duracaoProjetil = Finita 8.0}] ~=? sinergias gelo [gelo],
      "Gelo -> (Resina)" ~: [resina, gelo] ~=? sinergias gelo [resina],
      "Gelo -> (Gelo, Resina)" ~: [gelo {duracaoProjetil = Finita 8.0}, resina] ~=? sinergias gelo [gelo, resina],
      --
      "Resina -> ()" ~: [resina] ~=? sinergias resina [],
      "Resina -> (Fogo)" ~: [fogo {duracaoProjetil = Finita 8.0}] ~=? sinergias resina [fogo],
      "Resina -> (Gelo)" ~: [gelo, resina] ~=? sinergias resina [gelo],
      "Resina -> (Resina)" ~: [resina] ~=? sinergias resina [resina],
      "Resina -> (Gelo, Resina) " ~: [gelo, resina] ~=? sinergias resina [gelo, resina]
    ]
  where
    fogo = Projetil Fogo (Finita 4.0)
    gelo = Projetil Gelo (Finita 4.0)
    resina = Projetil Resina Infinita

jogo :: Jogo
jogo =
  Jogo
    { mapaDirecaoJogo =
        [ [Este, Sul, Repouso, Repouso, Repouso, Repouso],
          [Repouso, Sul, Repouso, Repouso, Repouso, Repouso],
          [Repouso, Sul, Repouso, Repouso, Repouso, Norte],
          [Repouso, Sul, Repouso, Repouso, Repouso, Norte],
          [Repouso, Este, Este, Este, Este, Norte],
          [Repouso, Repouso, Repouso, Repouso, Repouso, Repouso]
        ],
      baseJogo =
        Base
          { vidaBase = 100,
            posicaoBase = (0.5, 0.5),
            creditosBase = 500
          },
      portaisJogo =
        [ Portal
            { posicaoPortal = (5.5, 2.5),
              ondasPortal =
                [ Onda
                    { inimigosOnda =
                        [ Inimigo
                            { posicaoInimigo = (5.5, 2.5),
                              direcaoInimigo = Sul,
                              vidaInimigo = 80,
                              velocidadeInimigo = 1.0,
                              ataqueInimigo = 15,
                              butimInimigo = 10,
                              projeteisInimigo = [],
                              spritesInimigo = [Parado, AndarE, Parado, AndarD],
                              spriteCiclo = 0.3,
                              spriteDistancia = 0.3
                            }
                        ],
                      cicloOnda = 1.5,
                      tempoOnda = 0.5,
                      entradaOnda = 3
                    },
                  Onda
                    { inimigosOnda =
                        [ Inimigo
                            { posicaoInimigo = (5.5, 2.5),
                              direcaoInimigo = Este,
                              vidaInimigo = 80,
                              velocidadeInimigo = 1.0,
                              ataqueInimigo = 15,
                              butimInimigo = 10,
                              projeteisInimigo = [],
                              spritesInimigo = [Parado, AndarE, Parado, AndarD],
                              spriteCiclo = 0.3,
                              spriteDistancia = 0.3
                            }
                        ],
                      cicloOnda = 1.5,
                      tempoOnda = 0.5,
                      entradaOnda = 3
                    }
                ]
            }
        ],
      torresJogo =
        [ Torre
            { posicaoTorre = (4.5, 3.5),
              danoTorre = 50,
              alcanceTorre = 1.5,
              rajadaTorre = 3,
              cicloTorre = 2.0,
              tempoTorre = 0.0,
              projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
              defenidaTorre = True,
              menuAbertoTorre = False
            },
          Torre
            { posicaoTorre = (0, 2),
              danoTorre = 30,
              alcanceTorre = 4.0,
              rajadaTorre = 2,
              cicloTorre = 3.0,
              tempoTorre = 1.0,
              projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
              defenidaTorre = True,
              menuAbertoTorre = False
            }
        ],
      mapaJogo =
        [ [Terra, Terra, Relva, Agua, Agua, Agua],
          [Relva, Terra, Relva, Agua, Relva, Relva],
          [Relva, Terra, Relva, Agua, Relva, Terra],
          [Relva, Terra, Relva, Agua, Relva, Terra],
          [Relva, Terra, Terra, Terra, Terra, Terra],
          [Agua, Agua, Agua, Agua, Relva, Relva]
        ],
      inimigosJogo =
        [ Inimigo
            { posicaoInimigo = (1.5, 0.5),
              direcaoInimigo = Oeste,
              vidaInimigo = 50,
              velocidadeInimigo = 0.5,
              ataqueInimigo = 5,
              butimInimigo = 2,
              projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
              spritesInimigo = [Parado, AndarE, Parado, AndarD],
              spriteCiclo = 0.3,
              spriteDistancia = 0.3
            }
        ],
      lasersJogo = [],
      lojaJogo =
        [ ( 100,
            Torre
              { posicaoTorre = (0, 0),
                danoTorre = 20,
                alcanceTorre = 3.0,
                rajadaTorre = 1,
                cicloTorre = 2.0,
                tempoTorre = 0.0,
                projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0},
                defenidaTorre = True,
                menuAbertoTorre = False
              }
          )
        ]
    }