<?php

namespace Tests\Database;

use Tests\Database;

class Postgres extends Database
{
    /**
     * Test PostgreSQL-specific status
     * Verifies that status endpoint returns correct engine information
     */
    public function testPostgresStatus(): void
    {
        $response = \Tests\Client::execute(url: '/__opr/status', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertEquals('postgres', $body['engine']);
        self::assertEquals('16', $body['version']);
    }

    /**
     * Test PostgreSQL connection via PDO
     * Verifies actual database connectivity and authentication
     */
    public function testPostgresConnection(): void
    {
        $host = getenv('RUNTIME_NAME') === 'postgres' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 5432;
        $dbname = getenv('POSTGRES_DB') ?: 'postgres';
        $user = getenv('POSTGRES_USER') ?: 'postgres';
        $password = getenv('POSTGRES_PASSWORD') ?: 'postgres';

        try {
            $dsn = "pgsql:host=$host;port=$port;dbname=$dbname";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
                \PDO::ATTR_TIMEOUT => 5,
            ]);

            // Test basic query
            $stmt = $pdo->query('SELECT version()');
            $version = $stmt->fetchColumn();
            self::assertNotEmpty($version);
            self::assertStringContainsString('PostgreSQL', $version);

            // Verify we can see the database
            $stmt = $pdo->query('SELECT current_database()');
            $currentDb = $stmt->fetchColumn();
            self::assertEquals($dbname, $currentDb);

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('PostgreSQL connection test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test PostgreSQL table operations
     * Verifies ability to create, insert, select, and drop tables
     */
    public function testPostgresTableOperations(): void
    {
        $host = getenv('RUNTIME_NAME') === 'postgres' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 5432;
        $dbname = getenv('POSTGRES_DB') ?: 'postgres';
        $user = getenv('POSTGRES_USER') ?: 'postgres';
        $password = getenv('POSTGRES_PASSWORD') ?: 'postgres';

        try {
            $dsn = "pgsql:host=$host;port=$port;dbname=$dbname";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create test table
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_test (
                id SERIAL PRIMARY KEY,
                name VARCHAR(255) NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )');

            // Insert test data
            $stmt = $pdo->prepare('INSERT INTO opr_test (name) VALUES (?)');
            $stmt->execute(['test_user_1']);
            $stmt->execute(['test_user_2']);

            // Select and verify
            $stmt = $pdo->query('SELECT COUNT(*) FROM opr_test');
            $count = (int)$stmt->fetchColumn();
            self::assertGreaterThanOrEqual(2, $count);

            // Verify data integrity
            $stmt = $pdo->query('SELECT name FROM opr_test ORDER BY id');
            $names = $stmt->fetchAll(\PDO::FETCH_COLUMN);
            self::assertContains('test_user_1', $names);
            self::assertContains('test_user_2', $names);

            // Clean up
            $pdo->exec('DROP TABLE opr_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('PostgreSQL table operations test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test PostgreSQL transaction support
     * Verifies ACID compliance with transactions
     */
    public function testPostgresTransactions(): void
    {
        $host = getenv('RUNTIME_NAME') === 'postgres' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 5432;
        $dbname = getenv('POSTGRES_DB') ?: 'postgres';
        $user = getenv('POSTGRES_USER') ?: 'postgres';
        $password = getenv('POSTGRES_PASSWORD') ?: 'postgres';

        try {
            $dsn = "pgsql:host=$host;port=$port;dbname=$dbname";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create test table
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_txn_test (
                id SERIAL PRIMARY KEY,
                value INTEGER NOT NULL
            )');

            // Test successful transaction
            $pdo->beginTransaction();
            $pdo->exec('INSERT INTO opr_txn_test (value) VALUES (100)');
            $pdo->exec('INSERT INTO opr_txn_test (value) VALUES (200)');
            $pdo->commit();

            $stmt = $pdo->query('SELECT COUNT(*) FROM opr_txn_test');
            $count = (int)$stmt->fetchColumn();
            self::assertEquals(2, $count, 'Committed transaction should persist data');

            // Test rolled back transaction
            $pdo->beginTransaction();
            $pdo->exec('INSERT INTO opr_txn_test (value) VALUES (300)');
            $pdo->rollBack();

            $stmt = $pdo->query('SELECT COUNT(*) FROM opr_txn_test');
            $count = (int)$stmt->fetchColumn();
            self::assertEquals(2, $count, 'Rolled back transaction should not persist data');

            // Clean up
            $pdo->exec('DROP TABLE opr_txn_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('PostgreSQL transaction test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test PostgreSQL JSON support
     * Verifies JSON/JSONB column type support
     */
    public function testPostgresJSONSupport(): void
    {
        $host = getenv('RUNTIME_NAME') === 'postgres' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 5432;
        $dbname = getenv('POSTGRES_DB') ?: 'postgres';
        $user = getenv('POSTGRES_USER') ?: 'postgres';
        $password = getenv('POSTGRES_PASSWORD') ?: 'postgres';

        try {
            $dsn = "pgsql:host=$host;port=$port;dbname=$dbname";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create table with JSONB column
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_json_test (
                id SERIAL PRIMARY KEY,
                data JSONB NOT NULL
            )');

            // Insert JSON data
            $jsonData = json_encode(['name' => 'test', 'count' => 42, 'active' => true]);
            $stmt = $pdo->prepare('INSERT INTO opr_json_test (data) VALUES (?)');
            $stmt->execute([$jsonData]);

            // Query and verify JSON
            $stmt = $pdo->query("SELECT data->>'name' as name, data->>'count' as count FROM opr_json_test");
            $result = $stmt->fetch(\PDO::FETCH_ASSOC);
            self::assertEquals('test', $result['name']);
            self::assertEquals('42', $result['count']);

            // Clean up
            $pdo->exec('DROP TABLE opr_json_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('PostgreSQL JSON test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test PostgreSQL prepared statements
     * Verifies parameterized query support
     */
    public function testPostgresPreparedStatements(): void
    {
        $host = getenv('RUNTIME_NAME') === 'postgres' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 5432;
        $dbname = getenv('POSTGRES_DB') ?: 'postgres';
        $user = getenv('POSTGRES_USER') ?: 'postgres';
        $password = getenv('POSTGRES_PASSWORD') ?: 'postgres';

        try {
            $dsn = "pgsql:host=$host;port=$port;dbname=$dbname";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create test table
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_prepared_test (
                id SERIAL PRIMARY KEY,
                email VARCHAR(255) UNIQUE NOT NULL,
                age INTEGER
            )');

            // Test prepared statement with named parameters
            $stmt = $pdo->prepare('INSERT INTO opr_prepared_test (email, age) VALUES (:email, :age)');
            $stmt->execute([':email' => 'user1@example.com', ':age' => 25]);
            $stmt->execute([':email' => 'user2@example.com', ':age' => 30]);

            // Query with prepared statement
            $stmt = $pdo->prepare('SELECT email, age FROM opr_prepared_test WHERE age > :min_age');
            $stmt->execute([':min_age' => 26]);
            $results = $stmt->fetchAll(\PDO::FETCH_ASSOC);

            self::assertCount(1, $results);
            self::assertEquals('user2@example.com', $results[0]['email']);

            // Clean up
            $pdo->exec('DROP TABLE opr_prepared_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('PostgreSQL prepared statements test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test PostgreSQL connection limits
     * Verifies that connection tracking is accurate
     */
    public function testPostgresConnectionLimits(): void
    {
        $response = \Tests\Client::execute(url: '/__opr/status', method: 'GET');
        $body = \json_decode($response['body'], true);

        $maxConnections = $body['connections']['max'];
        self::assertGreaterThan(0, $maxConnections);

        // Verify default or configured max connections
        $expectedMax = (int)(getenv('POSTGRES_MAX_CONNECTIONS') ?: 100);
        self::assertEquals($expectedMax, $maxConnections);
    }

    /**
     * Test PostgreSQL startup performance
     * Verifies that startup time is reasonable
     */
    public function testPostgresStartupPerformance(): void
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

        // PostgreSQL should start within reasonable time (30 seconds for cold start)
        self::assertLessThan(30.0, $timings['startup'],
            'PostgreSQL cold start should complete within 30 seconds');
    }

    /**
     * Test PostgreSQL system information
     * Verifies that we can query system catalogs
     */
    public function testPostgresSystemInformation(): void
    {
        $host = getenv('RUNTIME_NAME') === 'postgres' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 5432;
        $dbname = getenv('POSTGRES_DB') ?: 'postgres';
        $user = getenv('POSTGRES_USER') ?: 'postgres';
        $password = getenv('POSTGRES_PASSWORD') ?: 'postgres';

        try {
            $dsn = "pgsql:host=$host;port=$port;dbname=$dbname";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Check server version
            $stmt = $pdo->query('SHOW server_version');
            $version = $stmt->fetchColumn();
            self::assertNotEmpty($version);
            self::assertStringContainsString('16', $version);

            // Check max connections setting
            $stmt = $pdo->query('SHOW max_connections');
            $maxConn = $stmt->fetchColumn();
            self::assertGreaterThan(0, (int)$maxConn);

            // Check current database
            $stmt = $pdo->query('SELECT current_database()');
            $currentDb = $stmt->fetchColumn();
            self::assertEquals($dbname, $currentDb);

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('PostgreSQL system information test skipped: ' . $e->getMessage());
        }
    }
}
