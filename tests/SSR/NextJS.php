<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class NextJS extends SSR
{
    public function testHomepagePrerendered(): void
    {
        $response = Client::execute(url: '/_next/image?url=https%3A%2F%2Fimages.unsplash.com%2Fphoto-1506744038136-46273834b3fb&w=828&q=75', method: 'GET');
        self::assertSame(200, $response['code']);

        $response = Client::execute(url: '/_next/image?url=https%3A%2F%2Fcdn.pixabay.com%2Fphoto%2F2020%2F11%2F10%2F01%2F34%2Fpet-5728249_1280.jpg&w=828&q=75', method: 'GET');
        self::assertSame(400, $response['code']);
    }
}
