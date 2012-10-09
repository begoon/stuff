<?php

require 'hex.php';

function decode_ucs2( $ch, $codepage = "w", $endian = "little" )
{
   $ucs2_cyr_cp1251_table = 
   // 0123456789ABCDEF
     " �              " . // 0
     "����������������" . // 1
     "����������������" . // 2
     "����������������" . // 3
     "����������������" . // 4
     " �              " . // 5
     "                " . // 6
     "                " . // 7
     "                " . // 8
     "                " . // 9
     "                " . // A
     "                " . // B
     "                " . // C
     "                " . // D
     "                " . // E
     "                ";  // F
   
   $ucs2_cyr_dos_table = 
   // 0123456789ABCDEF
     " �              " . // 0
     "����������������" . // 1
     "����������������" . // 2
     "����������������" . // 3
     "����������������" . // 4
     " �              " . // 5
     "                " . // 6
     "                " . // 7
     "                " . // 8
     "                " . // 9
     "                " . // A
     "                " . // B
     "                " . // C
     "                " . // D
     "                " . // E
     "                ";  // F
   
   if( strlen( $ch ) < 2 ) return $ch;
   
   if( $endian == "big" ) {
      $page = $ch{ 0 };
      $code = $ch{ 1 };
   } else {
      $page = $ch{ 1 };
      $code = $ch{ 0 };
   }
   
   if( $page == chr( 0x00 ) ) return $code;
   
   $table = $codepage == "w" ? $ucs2_cyr_cp1251_table : $ucs2_cyr_dos_table;
   
   if( $page == chr( 0x04 ) ) {
      $ch = $table[ ord( $code ) & 0xff ];
      return $ch == " " ? '?' : $ch;
   }
   
   return "<" . hex_dump( $ch ) . ">";
}

function decode_ucs2_string( $str, $codepage = "w", $endian = "little" )
{
   $result = "";
   
   for( $i = 0; $i < strlen( $str ); $i += 2 )
      $result .= decode_ucs2( substr( $str, $i, 2 ), $codepage, $endian );
      
   return $result;
}

?>
