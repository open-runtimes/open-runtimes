<?php
$data = json_decode(file_get_contents('composer.json'), true);
$data['name'] = 'user/function';
file_put_contents('composer.json', json_encode($data, JSON_PRETTY_PRINT));