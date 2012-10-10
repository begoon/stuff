<?php

function str2int( $str, $endian = "little" )
{
   $result = 0;
   
   if( $endian == "big" )
      for( $i = 0; $i < strlen( $str ); $i++ )
         $result = ( $result << 8 ) | ( ord( $str{ $i } ) & 0xff );
   else
      for( $i = strlen( $str ) - 1; $i >= 0 ; $i-- )
         $result = ( $result << 8 ) | ( ord( $str{ $i } ) & 0xff );

   return $result;
}

function extract_int( $str, &$i )
{
   $result = str2int( substr( $str, $i, 4 ) );
   
   $i += 4;
   
   return $result;
}

function extract_short( $str, &$i )
{
   $result = str2int( substr( $str, $i, 2 ) );
   
   $i += 2;
   
   return $result;
}

function extract_string( $str, &$i )
{
   $sz = extract_int( $str, $i ) * 2;

   $result = substr( $str, $i, $sz );

   $i += $sz;
    
   return decode_ucs2_string( $result, "d" );
}

?>
