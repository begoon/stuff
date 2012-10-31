<?

$f = file_get_contents("php://stdin");

echo gzencode($f);

?>