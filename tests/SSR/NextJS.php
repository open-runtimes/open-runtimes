<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class NextJS extends SSR
{
    public function testNft(): void
    {
        $archive = '/app/tests/.runtime/nft-build/src/code.tar.gz';
        $nftCount = (int)\shell_exec("tar -tzf {$archive} 2>/dev/null | grep -c 'node_modules/'");
        $metadata = (string)\shell_exec("tar -xOf {$archive} ./.open-runtimes 2>/dev/null");

        $archive = '/app/tests/.runtime/modclean-disabled-build/src/code.tar.gz';
        $fullCount = (int)\shell_exec("tar -tzf {$archive} 2>/dev/null | grep -c 'node_modules/'");

        self::assertGreaterThan(0, $fullCount, 'Expected files in node_modules when cleanup is disabled');
        self::assertLessThanOrEqual($fullCount, $nftCount, 'Expected Next.js NFT cleanup to not increase node_modules files');

        if (\str_ends_with($this->runtimeName, '_bun') || \str_ends_with($this->runtimeName, '_deno')) {
            self::assertStringContainsString('OPEN_RUNTIMES_CLEANUP=nft', $metadata);
            self::assertLessThan($fullCount, $nftCount, 'Expected fewer files in node_modules after Next.js NFT pruning');
        }
    }

    public function testHomepagePrerendered(): void
    {
        $response = Client::execute(url: '/_next/image?url=https%3A%2F%2Fimages.unsplash.com%2Fphoto-1506744038136-46273834b3fb&w=828&q=75', method: 'GET');
        self::assertEquals(200, $response['code']);

        $response = Client::execute(url: '/_next/image?url=https%3A%2F%2Fcdn.pixabay.com%2Fphoto%2F2020%2F11%2F10%2F01%2F34%2Fpet-5728249_1280.jpg&w=828&q=75', method: 'GET');
        self::assertEquals(400, $response['code']);
    }
}
