<?

$f = gzopen("php://stdin", "rb");

gzpassthru($f);

?>