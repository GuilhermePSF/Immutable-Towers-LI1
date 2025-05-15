module Imagem where

import Graphics.Gloss

-- | Estrutura que representa todas as imagens utilizadas no jogo.
data Imagens
  = Imagens
  { base :: Picture,            -- ^ Imagem da base.
    portal :: Picture,          -- ^ Imagem do portal.
    menuLoja :: Picture,        -- ^ Imagem do menu da loja.
    menuPrincipal :: Picture,   -- ^ Imagem do menu principal.
    menuControlos :: Picture,   -- ^ Imagem do menu de controlos.
    menuNiveis :: Picture,      -- ^ Imagem do menu de níveis.
    menuDerrota :: Picture,     -- ^ Imagem do menu de derrota.
    menuVitoria :: Picture,     -- ^ Imagem do menu de vitória.
    menuPausa :: Picture,       -- ^ Imagem do menu de pausa.
    moeda :: Picture,           -- ^ Imagem da moeda.
    torreGelo :: Picture,       -- ^ Imagem da torre de gelo.
    torreResina :: Picture,     -- ^ Imagem da torre de resina.
    torreFogo :: Picture,       -- ^ Imagem da torre de fogo.
    torreVeneno :: Picture,     -- ^ Imagem da torre de veneno.
    torreVazia :: Picture,      -- ^ Imagem da torre vazia.
    rua :: Picture,             -- ^ Imagem da rua.
    rio :: Picture,             -- ^ Imagem do rio.
    relva :: Picture,           -- ^ Imagem da relva.
    areia :: Picture,           -- ^ Imagem da areia.
    neve :: Picture,            -- ^ Imagem da neve.
    inimigoSulP :: Picture,     -- ^ Imagem do inimigo parado vindo do sul.
    inimigoNorteP :: Picture,   -- ^ Imagem do inimigo parado vindo do norte.
    inimigoEsteP :: Picture,    -- ^ Imagem do inimigo parado vindo do este.
    inimigoOesteP :: Picture,   -- ^ Imagem do inimigo parado vindo do oeste.
    inimigoSulE :: Picture,     -- ^ Imagem do inimigo com a perna Esquerda à frente vindo do sul.
    inimigoNorteE :: Picture,   -- ^ Imagem do inimigo com a perna Esquerda à frente vindo do norte.
    inimigoEsteE :: Picture,    -- ^ Imagem do inimigo com a perna Esquerda à frente vindo do este.
    inimigoOesteE :: Picture,   -- ^ Imagem do inimigo com a perna Esquerda à frente vindo do oeste.
    inimigoSulD :: Picture,     -- ^ Imagem do inimigo com a perna Direito à frente vindo do sul.
    inimigoNorteD :: Picture,   -- ^ Imagem do inimigo com a perna Direito à frente vindo do norte.
    inimigoEsteD :: Picture,    -- ^ Imagem do inimigo com a perna Direito à frente vindo do este.
    inimigoOesteD :: Picture,   -- ^ Imagem do inimigo com a perna Direito à frente vindo do oeste.
    zero :: Picture,            -- ^ Imagem do número zero.
    um :: Picture,              -- ^ Imagem do número um.
    dois :: Picture,            -- ^ Imagem do número dois.
    tres :: Picture,            -- ^ Imagem do número três.
    quatro :: Picture,          -- ^ Imagem do número quatro.
    cinco :: Picture,           -- ^ Imagem do número cinco.
    seis :: Picture,            -- ^ Imagem do número seis.
    sete :: Picture,            -- ^ Imagem do número sete.
    oito :: Picture,            -- ^ Imagem do número oito.
    nove :: Picture             -- ^ Imagem do número nove.
  }

-- Carrega todas as imagens necessárias para o jogo, escalando-as conforme o valor de `escalar`.
carregarImagem :: Float -> IO Imagens
carregarImagem escalar = do
  base <- loadBMP "images/misc/base.bmp"
  portal <- loadBMP "images/misc/portal.bmp"
  menuLoja <- loadBMP "images/misc/menuLoja.bmp"
  menuPrincipal <- loadBMP "images/menus/menuPrincipal.bmp"
  menuControlos <- loadBMP "images/menus/menuControlos.bmp"
  menuNiveis <- loadBMP "images/menus/menuNiveis.bmp"
  menuDerrota <- loadBMP "images/menus/menuDerrota.bmp"
  menuVitoria <- loadBMP "images/menus/menuVitoria.bmp"
  menuPausa <- loadBMP "images/menus/menuPausa.bmp"
  moeda <- loadBMP "images/misc/moeda.bmp"
  torreGelo <- loadBMP "images/torres/torreGelo.bmp"
  torreResina <- loadBMP "images/torres/torreResina.bmp"
  torreFogo <- loadBMP "images/torres/torreFogo.bmp"
  torreVeneno <- loadBMP "images/torres/torreVeneno.bmp"
  torreVazia <- loadBMP "images/torres/torreVazia.bmp"
  rua <- loadBMP "images/tiles/rua.bmp"
  rio <- loadBMP "images/tiles/rio.bmp"
  relva <- loadBMP "images/tiles/relva.bmp"
  areia <- loadBMP "images/tiles/areia.bmp"
  neve <- loadBMP "images/tiles/neve.bmp"
  inimigoSulP <- loadBMP "images/inimigo/sulP.bmp"
  inimigoNorteP <- loadBMP "images/inimigo/norteP.bmp"
  inimigoEsteP <- loadBMP "images/inimigo/esteP.bmp"
  inimigoOesteP <- loadBMP "images/inimigo/oesteP.bmp"
  inimigoSulE <- loadBMP "images/inimigo/sulE.bmp"
  inimigoNorteE <- loadBMP "images/inimigo/norteE.bmp"
  inimigoEsteE <- loadBMP "images/inimigo/esteE.bmp"
  inimigoOesteE <- loadBMP "images/inimigo/oesteE.bmp"
  inimigoSulD <- loadBMP "images/inimigo/sulD.bmp"
  inimigoNorteD <- loadBMP "images/inimigo/norteD.bmp"
  inimigoEsteD <- loadBMP "images/inimigo/esteD.bmp"
  inimigoOesteD <- loadBMP "images/inimigo/oesteD.bmp"
  zero <- loadBMP "images/numeros/zero.bmp"
  um <- loadBMP "images/numeros/um.bmp"
  dois <- loadBMP "images/numeros/dois.bmp"
  tres <- loadBMP "images/numeros/tres.bmp"
  quatro <- loadBMP "images/numeros/quatro.bmp"
  cinco <- loadBMP "images/numeros/cinco.bmp"
  seis <- loadBMP "images/numeros/seis.bmp"
  sete <- loadBMP "images/numeros/sete.bmp"
  oito <- loadBMP "images/numeros/oito.bmp"
  nove <- loadBMP "images/numeros/nove.bmp"
  return $
    Imagens
      { base = scale (0.74 * escalar) (0.74 * escalar) base,
        portal = scale (0.74 * escalar) (0.74 * escalar) portal,
        menuLoja = scale (2 * escalar) (2 * escalar) menuLoja,
        menuPrincipal = menuPrincipal,
        menuControlos = menuControlos,
        menuNiveis = menuNiveis,
        menuDerrota = menuDerrota,
        menuVitoria = menuVitoria,
        menuPausa = menuPausa,
        moeda = scale 1.5 1.5 moeda,
        torreGelo = scale (0.74 * escalar) (0.74 * escalar) torreGelo,
        torreResina = scale (0.74 * escalar) (0.74 * escalar) torreResina,
        torreFogo = scale (0.74 * escalar) (0.74 * escalar) torreFogo,
        torreVeneno = scale (0.74 * escalar) (0.74 * escalar) torreVeneno,
        torreVazia = scale escalar escalar torreVazia,
        rua = scale escalar escalar rua,
        rio = scale escalar escalar rio,
        relva = scale escalar escalar relva,
        areia = scale escalar escalar areia,
        neve = scale escalar escalar neve,
        inimigoSulP = scale (8 * escalar) (8 * escalar) inimigoSulP,
        inimigoNorteP = scale (8 * escalar) (8 * escalar) inimigoNorteP,
        inimigoEsteP = scale (8 * escalar) (8 * escalar) inimigoEsteP,
        inimigoOesteP = scale (8 * escalar) (8 * escalar) inimigoOesteP,
        inimigoSulE = scale (8 * escalar) (8 * escalar) inimigoSulE,
        inimigoNorteE = scale (8 * escalar) (8 * escalar) inimigoNorteE,
        inimigoEsteE = scale (8 * escalar) (8 * escalar) inimigoEsteE,
        inimigoOesteE = scale (8 * escalar) (8 * escalar) inimigoOesteE,
        inimigoSulD = scale (8 * escalar) (8 * escalar) inimigoSulD,
        inimigoNorteD = scale (8 * escalar) (8 * escalar) inimigoNorteD,
        inimigoEsteD = scale (8 * escalar) (8 * escalar) inimigoEsteD,
        inimigoOesteD = scale (8 * escalar) (8 * escalar) inimigoOesteD,
        zero = zero,
        um = um,
        dois = dois,
        tres = tres,
        quatro = quatro,
        cinco = cinco,
        seis = seis,
        sete = sete,
        oito = oito,
        nove = nove
      }