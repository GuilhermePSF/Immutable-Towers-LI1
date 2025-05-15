module Tarefa3Spec (testesTarefa3) where

import LI12425
import Tarefa2Spec (jogo)
import Tarefa3
import Test.HUnit

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ testAtualizaJogo,
        testAtualizaCooldown,
        testspawnInimigos,
        testAtualizarInimigosOnda,
        testretirarInimigoLancado,
        testinimigosALancar,
        testinimigoALancar,
        testdarCreditoInimigosVivos,
        testdanoBase,
        testdisparaTorres,
        testdisparaTorresAux,
        testdispara,
        testestaNoAlcance,
        testdanoEfeitoProjeteis,
        testremoveProjeteisExpirados,
        testsmoveInimigos,
        testmoveInmigosAux,
        testmudarDirecao,
        testvelPosProjeteis
      ]

testAtualizaJogo :: Test
testAtualizaJogo =
  TestList
    [ "testAtualizaJogo (jogo2)"
        ~: Jogo
          { baseJogo =
              Base
                { vidaBase = 100.0,
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
                              ],
                            cicloOnda = 1.5,
                            tempoOnda = 0.5,
                            entradaOnda = 2.0
                          },
                        Onda
                          { inimigosOnda =
                              [ Inimigo
                                  { posicaoInimigo = (5.5, 3.0),
                                    direcaoInimigo = Este,
                                    vidaInimigo = 80.0,
                                    velocidadeInimigo = 1.0,
                                    ataqueInimigo = 15.0,
                                    butimInimigo = 10,
                                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                                    spriteCiclo = 0.3,
                                    spriteDistancia = 0.3
                                  }
                              ],
                            cicloOnda = 1.5,
                            tempoOnda = 0.5,
                            entradaOnda = 2.0
                          }
                      ]
                  }
              ],
            torresJogo =
              [ Torre
                  { posicaoTorre = (4.5, 3.5),
                    danoTorre = 50.0,
                    alcanceTorre = 1.5,
                    rajadaTorre = 3,
                    cicloTorre = 2.0,
                    tempoTorre = 0.0,
                    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  },
                Torre
                  { posicaoTorre = (0.0, 2.0),
                    danoTorre = 30.0,
                    alcanceTorre = 4.0,
                    rajadaTorre = 2,
                    cicloTorre = 3.0,
                    tempoTorre = 0.0,
                    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  }
              ],
            mapaJogo = mapaJogo jogo2,
            mapaDirecaoJogo = mapaDirecaoJogo jogo2,
            inimigosJogo =
              [ Inimigo
                  { posicaoInimigo = (1.5, 0.0),
                    direcaoInimigo = Repouso,
                    vidaInimigo = 40.0,
                    velocidadeInimigo = 0.5,
                    ataqueInimigo = 5.0,
                    butimInimigo = 2,
                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = -0.19999999
                  }
              ],
            lasersJogo = [],
            lojaJogo = lojaJogo jogo2
          }
        ~=? atualizaJogo 1 jogo2,
      "testAtualizaJogo (jogo3)"
        ~: Jogo
          { baseJogo =
              Base
                { vidaBase = 95.0,
                  posicaoBase = (0.5, 0.5),
                  creditosBase = 510
                },
            portaisJogo =
              [ Portal
                  { posicaoPortal = (5.5, 2.5),
                    ondasPortal =
                      [ Onda
                          { inimigosOnda =
                              [ Inimigo
                                  { posicaoInimigo = (5.5, 3.0),
                                    direcaoInimigo = Este,
                                    vidaInimigo = 80.0,
                                    velocidadeInimigo = 1.0,
                                    ataqueInimigo = 15.0,
                                    butimInimigo = 10,
                                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                                    spriteCiclo = 0.3,
                                    spriteDistancia = 0.3
                                  }
                              ],
                            cicloOnda = 1.5,
                            tempoOnda = 0.5,
                            entradaOnda = -1.0
                          }
                      ]
                  }
              ],
            torresJogo =
              [ Torre
                  { posicaoTorre = (4.5, 3.5),
                    danoTorre = 50.0,
                    alcanceTorre = 1.5,
                    rajadaTorre = 3,
                    cicloTorre = 2.0,
                    tempoTorre = -2.0,
                    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  },
                Torre
                  { posicaoTorre = (0.0, 2.0),
                    danoTorre = 30.0,
                    alcanceTorre = 4.0,
                    rajadaTorre = 2,
                    cicloTorre = 3.0,
                    tempoTorre = -3.0,
                    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  }
              ],
            mapaJogo = mapaJogo jogo3,
            mapaDirecaoJogo = mapaDirecaoJogo jogo3,
            inimigosJogo = [],
            lasersJogo = [],
            lojaJogo = lojaJogo jogo3
          }
        ~=? atualizaJogo 4 jogo3
    ]

testAtualizaCooldown :: Test
testAtualizaCooldown =
  TestList
    [ "testAtualizaCooldown"
        ~: Jogo
          { baseJogo = baseJogo jogo2,
            portaisJogo =
              [ Portal
                  { posicaoPortal = (5.5, 2.5),
                    ondasPortal =
                      [ Onda
                          { inimigosOnda =
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
                                  }
                              ],
                            cicloOnda = 1.5,
                            tempoOnda = 0.5,
                            entradaOnda = 2.0
                          },
                        Onda
                          { inimigosOnda =
                              [ Inimigo
                                  { posicaoInimigo = (5.5, 3.0),
                                    direcaoInimigo = Este,
                                    vidaInimigo = 80.0,
                                    velocidadeInimigo = 1.0,
                                    ataqueInimigo = 15.0,
                                    butimInimigo = 10,
                                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                                    spriteCiclo = 0.3,
                                    spriteDistancia = 0.3
                                  }
                              ],
                            cicloOnda = 1.5,
                            tempoOnda = 0.5,
                            entradaOnda = 2.0
                          }
                      ]
                  }
              ],
            torresJogo =
              [ Torre
                  { posicaoTorre = (4.5, 3.5),
                    danoTorre = 50.0,
                    alcanceTorre = 1.5,
                    rajadaTorre = 3,
                    cicloTorre = 2.0,
                    tempoTorre = 0.0,
                    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  },
                Torre
                  { posicaoTorre = (0.0, 2.0),
                    danoTorre = 30.0,
                    alcanceTorre = 4.0,
                    rajadaTorre = 2,
                    cicloTorre = 3.0,
                    tempoTorre = 0.0,
                    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  }
              ],
            mapaJogo = mapaJogo jogo2,
            mapaDirecaoJogo = mapaDirecaoJogo jogo2,
            inimigosJogo =
              [ Inimigo
                  { posicaoInimigo = (1.5, 0.5),
                    direcaoInimigo = Oeste,
                    vidaInimigo = 50.0,
                    velocidadeInimigo = 0.5,
                    ataqueInimigo = 5.0,
                    butimInimigo = 2,
                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = -0.19999999
                  }
              ],
            lasersJogo = [],
            lojaJogo = lojaJogo jogo2
          }
        ~=? atualizaCooldown 1 jogo2
    ]

testspawnInimigos :: Test
testspawnInimigos =
  TestList
    [ "testspawnInimigos"
        ~: Jogo
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
                { vidaBase = 100.0,
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
                              ],
                            cicloOnda = 1.5,
                            tempoOnda = 0.5,
                            entradaOnda = 3.0
                          },
                        Onda
                          { inimigosOnda =
                              [ Inimigo
                                  { posicaoInimigo = (5.5, 3.0),
                                    direcaoInimigo = Este,
                                    vidaInimigo = 80.0,
                                    velocidadeInimigo = 1.0,
                                    ataqueInimigo = 15.0,
                                    butimInimigo = 10,
                                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                                    spriteCiclo = 0.3,
                                    spriteDistancia = 0.3
                                  }
                              ],
                            cicloOnda = 1.5,
                            tempoOnda = 0.5,
                            entradaOnda = 3.0
                          }
                      ]
                  }
              ],
            torresJogo =
              [ Torre
                  { posicaoTorre = (4.5, 3.5),
                    danoTorre = 50.0,
                    alcanceTorre = 1.5,
                    rajadaTorre = 3,
                    cicloTorre = 2.0,
                    tempoTorre = 0.0,
                    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  },
                Torre
                  { posicaoTorre = (0.0, 2.0),
                    danoTorre = 30.0,
                    alcanceTorre = 4.0,
                    rajadaTorre = 2,
                    cicloTorre = 3.0,
                    tempoTorre = 1.0,
                    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                    menuAbertoTorre = False,
                    defenidaTorre = False
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
                    vidaInimigo = 50.0,
                    velocidadeInimigo = 0.5,
                    ataqueInimigo = 5.0,
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
                    { posicaoTorre = (0.0, 0.0),
                      danoTorre = 20.0,
                      alcanceTorre = 3.0,
                      rajadaTorre = 1,
                      cicloTorre = 2.0,
                      tempoTorre = 0.0,
                      projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0},
                      menuAbertoTorre = False,
                      defenidaTorre = False
                    }
                )
              ]
          }
        ~=? spawnInimigos jogo2
    ]

testAtualizarInimigosOnda :: Test
testAtualizarInimigosOnda =
  TestList
    [ "AtualizarInimigosOnda"
        ~: Portal
          { posicaoPortal = (5.5, 2.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda = [],
                    cicloOnda = 1.5,
                    tempoOnda = 0.0,
                    entradaOnda = 0.0
                  },
                Onda
                  { inimigosOnda =
                      [ Inimigo
                          { posicaoInimigo = (5.5, 3.0),
                            direcaoInimigo = Este,
                            vidaInimigo = 80.0,
                            velocidadeInimigo = 1.0,
                            ataqueInimigo = 15.0,
                            butimInimigo = 10,
                            projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                            spritesInimigo = [Parado, AndarE, Parado, AndarD],
                            spriteCiclo = 0.3,
                            spriteDistancia = 0.3
                          }
                      ],
                    cicloOnda = 1.5,
                    tempoOnda = 0.5,
                    entradaOnda = 3.0
                  }
              ]
          }
        ~=? atualizarInimigosOnda (head (portaisJogo jogo3))
    ]

testretirarInimigoLancado :: Test
testretirarInimigoLancado =
  TestList
    [ "retirarInimigoLancado"
        ~: Onda
          { inimigosOnda =
              [ Inimigo
                  { posicaoInimigo = (5.5, 3.0),
                    direcaoInimigo = Este,
                    vidaInimigo = 80.0,
                    velocidadeInimigo = 1.0,
                    ataqueInimigo = 15.0,
                    butimInimigo = 10,
                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = 0.3
                  }
              ],
            cicloOnda = 1.5,
            tempoOnda = 0.0,
            entradaOnda = 0.0
          }
        ~=? retirarInimigoLancado o
    ]

testinimigosALancar :: Test
testinimigosALancar =
  TestList
    [ "inimigosALancar"
        ~: [ Inimigo
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
        ~=? inimigosALancar jogo3
    ]

testinimigoALancar :: Test
testinimigoALancar =
  TestList
    [ "inimigoALancar"
        ~: [ Inimigo
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
        ~=? inimigoALancar (head (portaisJogo jogo3))
    ]

testdarCreditoInimigosVivos :: Test
testdarCreditoInimigosVivos =
  TestList
    [ "darCreditoInimigosVivos"
        ~: Jogo
          { baseJogo =
              Base
                { vidaBase = 100.0,
                  posicaoBase = (0.5, 0.5),
                  creditosBase = 502
                },
            portaisJogo = portaisJogo jogo3,
            torresJogo = torresJogo jogo3,
            mapaJogo = mapaJogo jogo3,
            mapaDirecaoJogo = mapaDirecaoJogo jogo3,
            inimigosJogo = [],
            lasersJogo = [],
            lojaJogo = lojaJogo jogo3
          }
        ~=? darCreditoInimigosVivos jogo3
    ]

testdanoBase :: Test
testdanoBase =
  TestList
    [ "testdanoBase"
        ~: Jogo
          { baseJogo =
              Base
                { vidaBase = 95.0,
                  posicaoBase = (0.5, 0.5),
                  creditosBase = 500
                },
            portaisJogo = portaisJogo jogo3,
            torresJogo = torresJogo jogo3,
            mapaJogo = mapaJogo jogo3,
            mapaDirecaoJogo = mapaDirecaoJogo jogo3,
            inimigosJogo = [],
            lasersJogo = [],
            lojaJogo = lojaJogo jogo3
          }
        ~=? danoBase jogo3
    ]

testdisparaTorres :: Test
testdisparaTorres =
  TestList
    [ "testdisparaTorres"
        ~: Jogo
          { baseJogo = baseJogo jogo2,
            portaisJogo = portaisJogo jogo2,
            torresJogo =
              [ Torre
                  { posicaoTorre = (4.5, 3.5),
                    danoTorre = 50.0,
                    alcanceTorre = 1.5,
                    rajadaTorre = 3,
                    cicloTorre = 2.0,
                    tempoTorre = 2.0,
                    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  },
                Torre
                  { posicaoTorre = (0.0, 2.0),
                    danoTorre = 30.0,
                    alcanceTorre = 4.0,
                    rajadaTorre = 2,
                    cicloTorre = 3.0,
                    tempoTorre = 1.0,
                    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                    menuAbertoTorre = False,
                    defenidaTorre = False
                  }
              ],
            mapaJogo = mapaJogo jogo2,
            mapaDirecaoJogo = mapaDirecaoJogo jogo2,
            inimigosJogo =
              [ Inimigo
                  { posicaoInimigo = (4.5, 3.5),
                    direcaoInimigo = Oeste,
                    vidaInimigo = -110.0,
                    velocidadeInimigo = 0.5,
                    ataqueInimigo = 5.0,
                    butimInimigo = 2,
                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 32.0}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = 0.3
                  }
              ],
            lasersJogo = [Laser {posicaoTorreLaser = (4.5,3.5), posicaoInimigoLaser = (4.5,3.5), tipoLaser = Resina, tempoLaser = 0.5}],
            lojaJogo = lojaJogo jogo2
          }
        ~=? disparaTorres
          Jogo
            { baseJogo = baseJogo jogo2,
              portaisJogo = portaisJogo jogo2,
              torresJogo = torresJogo jogo2,
              mapaJogo = mapaJogo jogo2,
              mapaDirecaoJogo = mapaDirecaoJogo jogo2,
              inimigosJogo =
                [ Inimigo
                    { posicaoInimigo = (4.5, 3.5),
                      direcaoInimigo = Oeste,
                      vidaInimigo = 40.0,
                      velocidadeInimigo = 0.5,
                      ataqueInimigo = 5.0,
                      butimInimigo = 2,
                      projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                      spritesInimigo = [Parado, AndarE, Parado, AndarD],
                      spriteCiclo = 0.3,
                      spriteDistancia = 0.3
                    }
                ],
              lasersJogo = [],
              lojaJogo = lojaJogo jogo2
            }
    ]

testdisparaTorresAux :: Test
testdisparaTorresAux =
  TestList
    [ "testdisparaTorresAux"
        ~: [ Inimigo
               { posicaoInimigo = (1.5, 0.5),
                 direcaoInimigo = Oeste,
                 vidaInimigo = -10.0,
                 velocidadeInimigo = 0.5,
                 ataqueInimigo = 5.0,
                 butimInimigo = 2,
                 projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}],
                 spritesInimigo = [Parado, AndarE, Parado, AndarD],
                 spriteCiclo = 0.3,
                 spriteDistancia = 0.3
               }
           ]
        ~=? disparaTorresAux (torresJogo jogo2) (inimigosJogo jogo2)
    ]

testdispara :: Test
testdispara =
  TestList
    [ "testdispara"
        ~: [ Inimigo
               { posicaoInimigo = (1.5, 0.5),
                 direcaoInimigo = Oeste,
                 vidaInimigo = 50.0,
                 velocidadeInimigo = 0.5,
                 ataqueInimigo = 5.0,
                 butimInimigo = 2,
                 projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                 spritesInimigo = [Parado, AndarE, Parado, AndarD],
                 spriteCiclo = 0.3,
                 spriteDistancia = 0.3
               },
             Inimigo
               { posicaoInimigo = (1.5, 0.5),
                 direcaoInimigo = Oeste,
                 vidaInimigo = 50.0,
                 velocidadeInimigo = 0.5,
                 ataqueInimigo = 5.0,
                 butimInimigo = 2,
                 projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                 spritesInimigo = [Parado, AndarE, Parado, AndarD],
                 spriteCiclo = 0.3,
                 spriteDistancia = 0.3
               }
           ]
        ~=? dispara
          (t, 1)
          []
          (inimigosJogo jogo2)
          (inimigosJogo jogo2)
    ]

testestaNoAlcance :: Test
testestaNoAlcance =
  TestList
    [ "estaNoAlcance" ~: True ~=? estaNoAlcance t i
    ]

testdanoEfeitoProjeteis :: Test
testdanoEfeitoProjeteis =
  Jogo
    { baseJogo = baseJogo jogo2,
      portaisJogo = portaisJogo jogo2,
      torresJogo = torresJogo jogo2,
      mapaJogo = mapaJogo jogo2,
      mapaDirecaoJogo = mapaDirecaoJogo jogo2,
      inimigosJogo =
        [ Inimigo
            { posicaoInimigo = (1.5, 0.5),
              direcaoInimigo = Oeste,
              vidaInimigo = 40.0,
              velocidadeInimigo = 0.5,
              ataqueInimigo = 5.0,
              butimInimigo = 2,
              projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
              spritesInimigo = [Parado, AndarE, Parado, AndarD],
              spriteCiclo = 0.3,
              spriteDistancia = 0.3
            }
        ],
       lasersJogo = [],
      lojaJogo = lojaJogo jogo2
    }
    ~=? danoEfeitoProjeteis 1 jogo2

testremoveProjeteisExpirados :: Test
testremoveProjeteisExpirados =
  TestList
    [ "testremoveProjeteisExpirados"
        ~: Jogo
          { baseJogo = baseJogo jogo3,
            portaisJogo = portaisJogo jogo3,
            torresJogo = torresJogo jogo3,
            mapaJogo = mapaJogo jogo3,
            mapaDirecaoJogo = mapaDirecaoJogo jogo3,
            inimigosJogo =
              [ Inimigo
                  { posicaoInimigo = (0.7, 0.7),
                    direcaoInimigo = Oeste,
                    vidaInimigo = 0.0,
                    velocidadeInimigo = 0.5,
                    ataqueInimigo = 5.0,
                    butimInimigo = 2,
                    projeteisInimigo = [],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = 0.3
                  }
              ],
            lasersJogo = [],
            lojaJogo = lojaJogo jogo3
          }
        ~=? removeProjeteisExpirados jogo3
    ]

testsmoveInimigos :: Test
testsmoveInimigos =
  TestList
    [ "a"
        ~: jogoMovimento
          { inimigosJogo =
              [ Inimigo
                  { posicaoInimigo = (3.5, 1.3),
                    direcaoInimigo = Sul,
                    vidaInimigo = 50,
                    velocidadeInimigo = 0.1,
                    ataqueInimigo = 5,
                    butimInimigo = 2,
                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = 0.3
                  },
                Inimigo
                  { posicaoInimigo = (3.4, 1.3),
                    direcaoInimigo = Sul,
                    vidaInimigo = 50,
                    velocidadeInimigo = 0.1,
                    ataqueInimigo = 5,
                    butimInimigo = 2,
                    projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = 0.3
                  },
                Inimigo
                  { posicaoInimigo = (3.6000001, 1.3),
                    direcaoInimigo = Sul,
                    vidaInimigo = 50,
                    velocidadeInimigo = 0.4,
                    ataqueInimigo = 5,
                    butimInimigo = 2,
                    projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = 0.3
                  },
                Inimigo
                  { posicaoInimigo = (4.9, 1.7),
                    direcaoInimigo = Este,
                    vidaInimigo = 50,
                    velocidadeInimigo = 0.3,
                    ataqueInimigo = 5,
                    butimInimigo = 2,
                    projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
                    spritesInimigo = [Parado, AndarE, Parado, AndarD],
                    spriteCiclo = 0.3,
                    spriteDistancia = 0.3
                  }
              ]
          }
        ~=? moveInimigos 1 jogoMovimento
    ]

testmoveInmigosAux :: Test
testmoveInmigosAux =
  TestList
    [ "testmoveInimigosAux"
        ~: Inimigo
          { posicaoInimigo = (3.5, 3.8),
            direcaoInimigo = Sul,
            vidaInimigo = 50.0,
            velocidadeInimigo = 0.1,
            ataqueInimigo = 5.0,
            butimInimigo = 2,
            projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
            spritesInimigo = [Parado, AndarE, Parado, AndarD],
            spriteCiclo = 0.3,
            spriteDistancia = 0.3
          }
        ~=? moveInimigoAux 1 m i
    ]

testmudarDirecao :: Test
testmudarDirecao =
  TestList
    [ "testmudarDirecao" ~: Sul ~=? mudarDirecao m i
    ]

testvelPosProjeteis :: Test
testvelPosProjeteis =
  TestList
    [ "testVelPosProjeteis" ~: 0.5 ~=? velPosProjeteis projet 1
    ]

projet :: [Projetil]
projet = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}]

m :: MapaDirecional
m =
  [ [Este, Sul, Repouso, Repouso, Repouso, Repouso],
    [Repouso, Sul, Repouso, Repouso, Repouso, Repouso],
    [Repouso, Sul, Repouso, Repouso, Repouso, Norte],
    [Repouso, Sul, Repouso, Repouso, Repouso, Norte],
    [Repouso, Este, Este, Este, Este, Norte],
    [Repouso, Repouso, Repouso, Repouso, Repouso, Repouso]
  ]

t :: Torre
t =
  Torre
    { posicaoTorre = (4.5, 3.5),
      danoTorre = 50,
      alcanceTorre = 1.5,
      rajadaTorre = 3,
      cicloTorre = 2.0,
      tempoTorre = 0.0,
      projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
      menuAbertoTorre = False,
      defenidaTorre = False
    }

i :: Inimigo
i =
  Inimigo
    { posicaoInimigo = (3.4, 3.8),
      direcaoInimigo = Sul,
      vidaInimigo = 50,
      velocidadeInimigo = 0.1,
      ataqueInimigo = 5,
      butimInimigo = 2,
      projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
      spritesInimigo = [Parado, AndarE, Parado, AndarD],
      spriteCiclo = 0.3,
      spriteDistancia = 0.3
    }

o :: Onda
o =
  Onda
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
            },
          Inimigo
            { posicaoInimigo = (5.5, 3.0),
              direcaoInimigo = Este,
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
      tempoOnda = 0,
      entradaOnda = 0
    }

jogoMovimento :: Jogo
jogoMovimento =
  jogo
    { mapaDirecaoJogo =
        [ [Este, Sul, Repouso, Repouso, Repouso, Repouso],
          [Repouso, Sul, Repouso, Repouso, Repouso, Repouso],
          [Repouso, Sul, Repouso, Repouso, Repouso, Norte],
          [Repouso, Sul, Repouso, Repouso, Repouso, Norte],
          [Repouso, Este, Este, Este, Este, Norte],
          [Repouso, Repouso, Repouso, Repouso, Repouso, Repouso]
        ],
      inimigosJogo =
        [ Inimigo
            { posicaoInimigo = (3.4, 1.3),
              direcaoInimigo = Sul,
              vidaInimigo = 50,
              velocidadeInimigo = 0.1,
              ataqueInimigo = 5,
              butimInimigo = 2,
              projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
              spritesInimigo = [Parado, AndarE, Parado, AndarD],
              spriteCiclo = 0.3,
              spriteDistancia = 0.3
            },
          Inimigo
            { posicaoInimigo = (3.4, 1.3),
              direcaoInimigo = Sul,
              vidaInimigo = 50,
              velocidadeInimigo = 0.1,
              ataqueInimigo = 5,
              butimInimigo = 2,
              projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 4.0}],
              spritesInimigo = [Parado, AndarE, Parado, AndarD],
              spriteCiclo = 0.3,
              spriteDistancia = 0.3
            },
          Inimigo
            { posicaoInimigo = (3.4, 1.3),
              direcaoInimigo = Sul,
              vidaInimigo = 50,
              velocidadeInimigo = 0.4,
              ataqueInimigo = 5,
              butimInimigo = 2,
              projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
              spritesInimigo = [Parado, AndarE, Parado, AndarD],
              spriteCiclo = 0.3,
              spriteDistancia = 0.3
            },
          Inimigo
            { posicaoInimigo = (4.6, 1.7),
              direcaoInimigo = Sul,
              vidaInimigo = 50,
              velocidadeInimigo = 0.3,
              ataqueInimigo = 5,
              butimInimigo = 2,
              projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}],
              spritesInimigo = [Parado, AndarE, Parado, AndarD],
              spriteCiclo = 0.3,
              spriteDistancia = 0.3
            }
        ]
    }

jogo2 :: Jogo
jogo2 =
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
                    },
                  Onda
                    { inimigosOnda =
                        [ Inimigo
                            { posicaoInimigo = (5.5, 3.0),
                              direcaoInimigo = Este,
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
              menuAbertoTorre = False,
              defenidaTorre = False
            },
          Torre
            { posicaoTorre = (0, 2),
              danoTorre = 30,
              alcanceTorre = 4.0,
              rajadaTorre = 2,
              cicloTorre = 3.0,
              tempoTorre = 1.0,
              projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
              menuAbertoTorre = False,
              defenidaTorre = False
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
                menuAbertoTorre = False,
                defenidaTorre = False
              }
          )
        ]
    }

jogo3 :: Jogo
jogo3 =
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
                      tempoOnda = 0,
                      entradaOnda = 0
                    },
                  Onda
                    { inimigosOnda =
                        [ Inimigo
                            { posicaoInimigo = (5.5, 3.0),
                              direcaoInimigo = Este,
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
              menuAbertoTorre = False,
              defenidaTorre = False
            },
          Torre
            { posicaoTorre = (0, 2),
              danoTorre = 30,
              alcanceTorre = 4.0,
              rajadaTorre = 2,
              cicloTorre = 3.0,
              tempoTorre = 1.0,
              projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
              menuAbertoTorre = False,
              defenidaTorre = False
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
            { posicaoInimigo = (0.7, 0.7),
              direcaoInimigo = Oeste,
              vidaInimigo = 0,
              velocidadeInimigo = 0.5,
              ataqueInimigo = 5,
              butimInimigo = 2,
              projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 0}],
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
                menuAbertoTorre = False,
                defenidaTorre = False
              }
          )
        ]
    }
