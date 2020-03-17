SELECT cod_emisor
      FROM clr_emisores
     WHERE cod_emisor NOT IN ('0105', '0108');

INSERT into clr_emisores (COD_EMISOR) Values ('9001');
INSERT into clr_emisores (COD_EMISOR) Values ('9010');

--delete from clr_emisores  where COD_EMISOR='9010';
--delete from clr_emisores  where COD_EMISOR='9001';