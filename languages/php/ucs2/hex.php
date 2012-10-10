<?php

function hex_dump( $str )
{
   $result = unpack( "H" . strlen( $str )*2 . "dump", $str );
   return strtoupper( $result[ "dump" ] );
}

function pack_hex( $str )
{
   return pack( "H*", strtoupper( preg_replace( "/[^\dABCDEF]/", "", $str ) ) );
}

function smart_hex_dump( $data )
{
   $dump = false;
   
   $result = "";
   
   for( $i = 0; $i < strlen( $data ); $i++ ) {
      $ch = ord( $data{ $i } );
      
      if( $dump ) {
         if( $ch < 0x20 || $ch > 0x7f || chr( $ch ) == ">" || chr( $ch ) == "<" )
            $result .= sprintf( "%02X", $ch );
         else {
            $result .= ">" . chr( $ch );
            $dump = false;
         }
      } else {
         if( $ch < 0x20 || $ch > 0x7f || chr( $ch ) == ">" || chr( $ch ) == "<" ) {
            $result .= "<" . sprintf( "%02X", $ch );
            $dump = true;
         } else
            $result .= chr( $ch );
      }
   }
   
   if( $dump )
      $result .= ">";
      
   return $result;
}
  
?>