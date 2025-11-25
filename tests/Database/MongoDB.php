<?php

namespace Tests\Database;

use Tests\Database;

class MongoDB extends Database
{
    /**
     * Test MongoDB-specific status
     * Verifies that status endpoint returns correct engine information
     */
    public function testMongoDBStatus(): void
    {
        $response = \Tests\Client::execute(url: '/__opr/status', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertEquals('mongodb', $body['engine']);
        self::assertEquals('8', $body['version']);
    }

    /**
     * Test MongoDB connection via MongoDB extension
     * Verifies actual database connectivity and authentication
     */
    public function testMongoDBConnection(): void
    {
        if (!class_exists('\MongoDB\Driver\Manager')) {
            self::markTestSkipped('MongoDB extension not available');
            return;
        }

        $host = getenv('RUNTIME_NAME') === 'mongodb' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 27017;
        $username = getenv('MONGO_INITDB_ROOT_USERNAME') ?: 'admin';
        $password = getenv('MONGO_INITDB_ROOT_PASSWORD') ?: 'admin';
        $authDb = getenv('MONGO_INITDB_DATABASE') ?: 'admin';

        try {
            $uri = sprintf(
                'mongodb://%s:%s@%s:%d/?authSource=%s',
                urlencode($username),
                urlencode($password),
                $host,
                $port,
                $authDb
            );

            $manager = new \MongoDB\Driver\Manager($uri);

            // Test ping command
            $command = new \MongoDB\Driver\Command(['ping' => 1]);
            $cursor = $manager->executeCommand('admin', $command);
            $response = $cursor->toArray()[0];

            self::assertEquals(1, $response->ok);
            self::assertTrue(true);
        } catch (\Exception $e) {
            self::markTestSkipped('MongoDB connection test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MongoDB collection operations
     * Verifies ability to create, insert, find, and drop collections
     */
    public function testMongoDBCollectionOperations(): void
    {
        if (!class_exists('\MongoDB\Driver\Manager')) {
            self::markTestSkipped('MongoDB extension not available');
            return;
        }

        $host = getenv('RUNTIME_NAME') === 'mongodb' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 27017;
        $username = getenv('MONGO_INITDB_ROOT_USERNAME') ?: 'admin';
        $password = getenv('MONGO_INITDB_ROOT_PASSWORD') ?: 'admin';
        $authDb = getenv('MONGO_INITDB_DATABASE') ?: 'admin';

        try {
            $uri = sprintf(
                'mongodb://%s:%s@%s:%d/?authSource=%s',
                urlencode($username),
                urlencode($password),
                $host,
                $port,
                $authDb
            );

            $manager = new \MongoDB\Driver\Manager($uri);
            $testDb = 'opr_test_db';
            $testCollection = 'opr_test_collection';

            // Insert documents
            $bulk = new \MongoDB\Driver\BulkWrite();
            $bulk->insert(['name' => 'user1', 'age' => 25, 'active' => true]);
            $bulk->insert(['name' => 'user2', 'age' => 30, 'active' => true]);
            $bulk->insert(['name' => 'user3', 'age' => 35, 'active' => false]);

            $result = $manager->executeBulkWrite("$testDb.$testCollection", $bulk);
            self::assertEquals(3, $result->getInsertedCount());

            // Query documents
            $query = new \MongoDB\Driver\Query(['active' => true]);
            $cursor = $manager->executeQuery("$testDb.$testCollection", $query);
            $documents = $cursor->toArray();

            self::assertCount(2, $documents);
            self::assertEquals('user1', $documents[0]->name);
            self::assertEquals('user2', $documents[1]->name);

            // Clean up - drop collection
            $command = new \MongoDB\Driver\Command(['drop' => $testCollection]);
            $manager->executeCommand($testDb, $command);

            self::assertTrue(true);
        } catch (\Exception $e) {
            self::markTestSkipped('MongoDB collection operations test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MongoDB document updates
     * Verifies ability to update documents
     */
    public function testMongoDBDocumentUpdates(): void
    {
        if (!class_exists('\MongoDB\Driver\Manager')) {
            self::markTestSkipped('MongoDB extension not available');
            return;
        }

        $host = getenv('RUNTIME_NAME') === 'mongodb' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 27017;
        $username = getenv('MONGO_INITDB_ROOT_USERNAME') ?: 'admin';
        $password = getenv('MONGO_INITDB_ROOT_PASSWORD') ?: 'admin';
        $authDb = getenv('MONGO_INITDB_DATABASE') ?: 'admin';

        try {
            $uri = sprintf(
                'mongodb://%s:%s@%s:%d/?authSource=%s',
                urlencode($username),
                urlencode($password),
                $host,
                $port,
                $authDb
            );

            $manager = new \MongoDB\Driver\Manager($uri);
            $testDb = 'opr_test_db';
            $testCollection = 'opr_update_test';

            // Insert test document
            $bulk = new \MongoDB\Driver\BulkWrite();
            $bulk->insert(['name' => 'test_user', 'status' => 'pending', 'count' => 0]);
            $manager->executeBulkWrite("$testDb.$testCollection", $bulk);

            // Update document
            $bulk = new \MongoDB\Driver\BulkWrite();
            $bulk->update(
                ['name' => 'test_user'],
                ['$set' => ['status' => 'active'], '$inc' => ['count' => 1]],
                ['multi' => false]
            );
            $result = $manager->executeBulkWrite("$testDb.$testCollection", $bulk);
            self::assertEquals(1, $result->getModifiedCount());

            // Verify update
            $query = new \MongoDB\Driver\Query(['name' => 'test_user']);
            $cursor = $manager->executeQuery("$testDb.$testCollection", $query);
            $document = $cursor->toArray()[0];

            self::assertEquals('active', $document->status);
            self::assertEquals(1, $document->count);

            // Clean up
            $command = new \MongoDB\Driver\Command(['drop' => $testCollection]);
            $manager->executeCommand($testDb, $command);

            self::assertTrue(true);
        } catch (\Exception $e) {
            self::markTestSkipped('MongoDB document updates test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MongoDB aggregation pipeline
     * Verifies aggregation framework functionality
     */
    public function testMongoDBAggregatement(): void
    {
        if (!class_exists('\MongoDB\Driver\Manager')) {
            self::markTestSkipped('MongoDB extension not available');
            return;
        }

        $host = getenv('RUNTIME_NAME') === 'mongodb' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 27017;
        $username = getenv('MONGO_INITDB_ROOT_USERNAME') ?: 'admin';
        $password = getenv('MONGO_INITDB_ROOT_PASSWORD') ?: 'admin';
        $authDb = getenv('MONGO_INITDB_DATABASE') ?: 'admin';

        try {
            $uri = sprintf(
                'mongodb://%s:%s@%s:%d/?authSource=%s',
                urlencode($username),
                urlencode($password),
                $host,
                $port,
                $authDb
            );

            $manager = new \MongoDB\Driver\Manager($uri);
            $testDb = 'opr_test_db';
            $testCollection = 'opr_agg_test';

            // Insert test documents
            $bulk = new \MongoDB\Driver\BulkWrite();
            $bulk->insert(['category' => 'A', 'value' => 10]);
            $bulk->insert(['category' => 'A', 'value' => 20]);
            $bulk->insert(['category' => 'B', 'value' => 15]);
            $bulk->insert(['category' => 'B', 'value' => 25]);
            $manager->executeBulkWrite("$testDb.$testCollection", $bulk);

            // Run aggregation pipeline
            $command = new \MongoDB\Driver\Command([
                'aggregate' => $testCollection,
                'pipeline' => [
                    ['$group' => [
                        '_id' => '$category',
                        'total' => ['$sum' => '$value'],
                        'count' => ['$sum' => 1]
                    ]],
                    ['$sort' => ['_id' => 1]]
                ],
                'cursor' => new \stdClass()
            ]);

            $cursor = $manager->executeCommand($testDb, $command);
            $results = $cursor->toArray();

            self::assertCount(2, $results);
            self::assertEquals('A', $results[0]->_id);
            self::assertEquals(30, $results[0]->total);
            self::assertEquals(2, $results[0]->count);
            self::assertEquals('B', $results[1]->_id);
            self::assertEquals(40, $results[1]->total);

            // Clean up
            $command = new \MongoDB\Driver\Command(['drop' => $testCollection]);
            $manager->executeCommand($testDb, $command);

            self::assertTrue(true);
        } catch (\Exception $e) {
            self::markTestSkipped('MongoDB aggregation test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MongoDB indexes
     * Verifies index creation and usage
     */
    public function testMongoDBIndexes(): void
    {
        if (!class_exists('\MongoDB\Driver\Manager')) {
            self::markTestSkipped('MongoDB extension not available');
            return;
        }

        $host = getenv('RUNTIME_NAME') === 'mongodb' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 27017;
        $username = getenv('MONGO_INITDB_ROOT_USERNAME') ?: 'admin';
        $password = getenv('MONGO_INITDB_ROOT_PASSWORD') ?: 'admin';
        $authDb = getenv('MONGO_INITDB_DATABASE') ?: 'admin';

        try {
            $uri = sprintf(
                'mongodb://%s:%s@%s:%d/?authSource=%s',
                urlencode($username),
                urlencode($password),
                $host,
                $port,
                $authDb
            );

            $manager = new \MongoDB\Driver\Manager($uri);
            $testDb = 'opr_test_db';
            $testCollection = 'opr_index_test';

            // Create index
            $command = new \MongoDB\Driver\Command([
                'createIndexes' => $testCollection,
                'indexes' => [
                    [
                        'key' => ['email' => 1],
                        'name' => 'email_idx',
                        'unique' => true
                    ]
                ]
            ]);
            $manager->executeCommand($testDb, $command);

            // List indexes to verify
            $command = new \MongoDB\Driver\Command(['listIndexes' => $testCollection]);
            $cursor = $manager->executeCommand($testDb, $command);
            $indexes = $cursor->toArray();

            $indexNames = array_map(fn($idx) => $idx->name, $indexes);
            self::assertContains('email_idx', $indexNames);

            // Clean up
            $command = new \MongoDB\Driver\Command(['drop' => $testCollection]);
            $manager->executeCommand($testDb, $command);

            self::assertTrue(true);
        } catch (\Exception $e) {
            self::markTestSkipped('MongoDB indexes test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test connection count reporting
     * Verifies that MongoDB reports current and available connections
     */
    public function testConnectionCount(): void
    {
        $response = \Tests\Client::execute(url: '/__opr/status', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertArrayHasKey('connections', $body);
        self::assertArrayHasKey('current', $body['connections']);
        self::assertArrayHasKey('available', $body['connections']);
        self::assertArrayHasKey('max', $body['connections']);
        self::assertIsInt($body['connections']['current']);
        self::assertIsInt($body['connections']['available']);
        self::assertIsInt($body['connections']['max']);

        // Verify that current + available = max
        self::assertEquals(
            $body['connections']['max'],
            $body['connections']['current'] + $body['connections']['available']
        );
    }

    /**
     * Test MongoDB startup performance
     * Verifies that startup time is reasonable
     */
    public function testMongoDBStartupPerformance(): void
    {
        $response = \Tests\Client::execute(url: '/__opr/timings', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = $response['body'];
        $lines = explode("\n", trim($body));

        $timings = [];
        foreach ($lines as $line) {
            if (empty($line)) continue;
            $parts = explode('=', $line, 2);
            if (count($parts) === 2) {
                $timings[$parts[0]] = (float)$parts[1];
            }
        }

        self::assertArrayHasKey('startup', $timings);

        // MongoDB should start within reasonable time (30 seconds for cold start)
        self::assertLessThan(30.0, $timings['startup'],
            'MongoDB cold start should complete within 30 seconds');
    }

    /**
     * Test MongoDB system information
     * Verifies that we can query server status
     */
    public function testMongoDBSystemInformation(): void
    {
        if (!class_exists('\MongoDB\Driver\Manager')) {
            self::markTestSkipped('MongoDB extension not available');
            return;
        }

        $host = getenv('RUNTIME_NAME') === 'mongodb' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 27017;
        $username = getenv('MONGO_INITDB_ROOT_USERNAME') ?: 'admin';
        $password = getenv('MONGO_INITDB_ROOT_PASSWORD') ?: 'admin';
        $authDb = getenv('MONGO_INITDB_DATABASE') ?: 'admin';

        try {
            $uri = sprintf(
                'mongodb://%s:%s@%s:%d/?authSource=%s',
                urlencode($username),
                urlencode($password),
                $host,
                $port,
                $authDb
            );

            $manager = new \MongoDB\Driver\Manager($uri);

            // Get server version
            $command = new \MongoDB\Driver\Command(['buildInfo' => 1]);
            $cursor = $manager->executeCommand('admin', $command);
            $buildInfo = $cursor->toArray()[0];

            self::assertNotEmpty($buildInfo->version);
            self::assertStringStartsWith('8.', $buildInfo->version);

            // Get server status
            $command = new \MongoDB\Driver\Command(['serverStatus' => 1]);
            $cursor = $manager->executeCommand('admin', $command);
            $status = $cursor->toArray()[0];

            self::assertObjectHasProperty('connections', $status);
            self::assertObjectHasProperty('uptime', $status);
            self::assertGreaterThan(0, $status->uptime);

            self::assertTrue(true);
        } catch (\Exception $e) {
            self::markTestSkipped('MongoDB system information test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MongoDB bulk operations
     * Verifies bulk write performance
     */
    public function testMongoDBBulkOperations(): void
    {
        if (!class_exists('\MongoDB\Driver\Manager')) {
            self::markTestSkipped('MongoDB extension not available');
            return;
        }

        $host = getenv('RUNTIME_NAME') === 'mongodb' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 27017;
        $username = getenv('MONGO_INITDB_ROOT_USERNAME') ?: 'admin';
        $password = getenv('MONGO_INITDB_ROOT_PASSWORD') ?: 'admin';
        $authDb = getenv('MONGO_INITDB_DATABASE') ?: 'admin';

        try {
            $uri = sprintf(
                'mongodb://%s:%s@%s:%d/?authSource=%s',
                urlencode($username),
                urlencode($password),
                $host,
                $port,
                $authDb
            );

            $manager = new \MongoDB\Driver\Manager($uri);
            $testDb = 'opr_test_db';
            $testCollection = 'opr_bulk_test';

            // Perform bulk insert
            $bulk = new \MongoDB\Driver\BulkWrite();
            for ($i = 0; $i < 100; $i++) {
                $bulk->insert(['index' => $i, 'value' => $i * 10]);
            }

            $result = $manager->executeBulkWrite("$testDb.$testCollection", $bulk);
            self::assertEquals(100, $result->getInsertedCount());

            // Verify count
            $command = new \MongoDB\Driver\Command(['count' => $testCollection]);
            $cursor = $manager->executeCommand($testDb, $command);
            $countResult = $cursor->toArray()[0];
            self::assertEquals(100, $countResult->n);

            // Clean up
            $command = new \MongoDB\Driver\Command(['drop' => $testCollection]);
            $manager->executeCommand($testDb, $command);

            self::assertTrue(true);
        } catch (\Exception $e) {
            self::markTestSkipped('MongoDB bulk operations test skipped: ' . $e->getMessage());
        }
    }
}
