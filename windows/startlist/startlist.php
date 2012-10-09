<?php

function print_reg_run( $type )
{
   echo "$type\n";
   echo str_repeat( "-", strlen($type) ), "\n";
   $cmd = "regedit /ea reg.tmp $type\Software\Microsoft\Windows\CurrentVersion\Run && type reg.tmp && del reg.tmp";

   $is = `$cmd`;

   $is = preg_replace( "/\r/", "", $is );
   $is = preg_replace( "/\\\\\"/", "", $is );

   preg_match( "/Run\](.*?)\n\n/s", $is, $matches );
   $is = $matches[ 1 ];

   preg_match_all( '/"(.+?)"="(.+?)"/', $is, $matches );

   for( $i = 0; $i < count( $matches[1] ); $i++ )
      echo $matches[1][$i], str_repeat( " ", 30 - strlen($matches[1][$i]) ), str_replace( "\\\\", "\\", $matches[2][$i] ), "\n";

   echo "\n";
}

function print_lnk( $type )
{
   echo "$type\n";
   echo str_repeat( "-", strlen($type) ), "\n";

   $cmd = "cmd /c dir /b \"$type\"";
   exec( $cmd, $out );
   foreach( $out as $line ) {
      $file = trim( `echo $type` ) . "\\" . trim( $line );
      echo $line, " => ", `lnkparse "$file"`, "\n";
   }
   echo "\n";
}

print_reg_run( 'HKEY_CURRENT_USER' );
print_reg_run( 'HKEY_LOCAL_MACHINE' );

print_lnk( "%USERPROFILE%\Start Menu\Programs\Startup" );
print_lnk( "%ALLUSERSPROFILE%\Start Menu\Programs\Startup" );
print_lnk( "%windir%\system32\config\systemprofile\Start Menu\Programs\Startup" );

?>
