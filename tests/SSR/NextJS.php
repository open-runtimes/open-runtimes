<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class NextJS extends SSR
{
    // NFT prunes Next.js non-standalone builds via synthetic entry + .nft.json merge
    public function testNft(): void
    {
        $archive = '/app/tests/.runtime/nft-build/src/code.tar.gz';
        $nftCount = (int)\shell_exec("tar -tzf {$archive} 2>/dev/null | grep -c 'node_modules/'");

        $archive = '/app/tests/.runtime/modclean-disabled-build/src/code.tar.gz';
        $fullCount = (int)\shell_exec("tar -tzf {$archive} 2>/dev/null | grep -c 'node_modules/'");

        self::assertGreaterThan(0, $fullCount, 'Expected files in node_modules when cleanup is disabled');
        self::assertLessThan($fullCount, $nftCount, 'Expected NFT to prune node_modules for Next.js');
    }

    public function testHomepagePrerendered(): void
    {
        $response = Client::execute(url: '/_next/image?url=https%3A%2F%2Fimages.unsplash.com%2Fphoto-1506744038136-46273834b3fb&w=828&q=75', method: 'GET');
        self::assertEquals(200, $response['code']);

        $response = Client::execute(url: '/_next/image?url=https%3A%2F%2Fcdn.pixabay.com%2Fphoto%2F2020%2F11%2F10%2F01%2F34%2Fpet-5728249_1280.jpg&w=828&q=75', method: 'GET');
        self::assertEquals(400, $response['code']);
    }
}
