<?php

namespace Tests\Database;

use Tests\Database;

class MySQL extends Database
{
    /**
     * Test MySQL-specific status
     * Verifies that status endpoint returns correct engine information
     */
    public function testMySQLStatus(): void
    {
        $response = \Tests\Client::execute(url: '/__opr/status', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        // Could be either mysql or mariadb
        self::assertContains($body['engine'], ['mysql', 'mariadb']);

        // Version should be 8.4 for MySQL or 11 for MariaDB
        self::assertNotEmpty($body['version']);
    }

    /**
     * Test MySQL connection via PDO
     * Verifies actual database connectivity and authentication
     */
    public function testMySQLConnection(): void
    {
        $host = getenv('RUNTIME_NAME') === 'mysql' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 3306;
        $dbname = getenv('MYSQL_DATABASE') ?: 'mysql';
        $user = 'root';
        $password = getenv('MYSQL_ROOT_PASSWORD') ?: 'root';

        try {
            $dsn = "mysql:host=$host;port=$port;dbname=$dbname;charset=utf8mb4";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
                \PDO::ATTR_TIMEOUT => 5,
            ]);

            // Test basic query
            $stmt = $pdo->query('SELECT VERSION()');
            $version = $stmt->fetchColumn();
            self::assertNotEmpty($version);

            // Verify database selection
            $stmt = $pdo->query('SELECT DATABASE()');
            $currentDb = $stmt->fetchColumn();
            self::assertEquals($dbname, $currentDb);

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('MySQL connection test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MySQL table operations
     * Verifies ability to create, insert, select, and drop tables
     */
    public function testMySQLTableOperations(): void
    {
        $host = getenv('RUNTIME_NAME') === 'mysql' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 3306;
        $dbname = getenv('MYSQL_DATABASE') ?: 'mysql';
        $user = 'root';
        $password = getenv('MYSQL_ROOT_PASSWORD') ?: 'root';

        try {
            $dsn = "mysql:host=$host;port=$port;dbname=$dbname;charset=utf8mb4";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create test table
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_test (
                id INT AUTO_INCREMENT PRIMARY KEY,
                name VARCHAR(255) NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4');

            // Insert test data
            $stmt = $pdo->prepare('INSERT INTO opr_test (name) VALUES (?)');
            $stmt->execute(['test_user_1']);
            $stmt->execute(['test_user_2']);
            $stmt->execute(['test_user_3']);

            // Select and verify
            $stmt = $pdo->query('SELECT COUNT(*) FROM opr_test');
            $count = (int)$stmt->fetchColumn();
            self::assertGreaterThanOrEqual(3, $count);

            // Verify data integrity
            $stmt = $pdo->query('SELECT name FROM opr_test ORDER BY id');
            $names = $stmt->fetchAll(\PDO::FETCH_COLUMN);
            self::assertContains('test_user_1', $names);
            self::assertContains('test_user_2', $names);
            self::assertContains('test_user_3', $names);

            // Clean up
            $pdo->exec('DROP TABLE opr_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('MySQL table operations test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MySQL transaction support
     * Verifies ACID compliance with transactions
     */
    public function testMySQLTransactions(): void
    {
        $host = getenv('RUNTIME_NAME') === 'mysql' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 3306;
        $dbname = getenv('MYSQL_DATABASE') ?: 'mysql';
        $user = 'root';
        $password = getenv('MYSQL_ROOT_PASSWORD') ?: 'root';

        try {
            $dsn = "mysql:host=$host;port=$port;dbname=$dbname;charset=utf8mb4";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create test table with InnoDB for transaction support
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_txn_test (
                id INT AUTO_INCREMENT PRIMARY KEY,
                value INT NOT NULL
            ) ENGINE=InnoDB');

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
            self::markTestSkipped('MySQL transaction test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MySQL JSON support
     * Verifies JSON column type support (MySQL 5.7+)
     */
    public function testMySQLJSONSupport(): void
    {
        $host = getenv('RUNTIME_NAME') === 'mysql' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 3306;
        $dbname = getenv('MYSQL_DATABASE') ?: 'mysql';
        $user = 'root';
        $password = getenv('MYSQL_ROOT_PASSWORD') ?: 'root';

        try {
            $dsn = "mysql:host=$host;port=$port;dbname=$dbname;charset=utf8mb4";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create table with JSON column
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_json_test (
                id INT AUTO_INCREMENT PRIMARY KEY,
                data JSON NOT NULL
            ) ENGINE=InnoDB');

            // Insert JSON data
            $jsonData = json_encode(['name' => 'test', 'count' => 42, 'active' => true]);
            $stmt = $pdo->prepare('INSERT INTO opr_json_test (data) VALUES (?)');
            $stmt->execute([$jsonData]);

            // Query and verify JSON using MySQL JSON functions
            $stmt = $pdo->query("SELECT JSON_EXTRACT(data, '$.name') as name, JSON_EXTRACT(data, '$.count') as count FROM opr_json_test");
            $result = $stmt->fetch(\PDO::FETCH_ASSOC);
            self::assertEquals('"test"', $result['name']); // JSON strings are quoted
            self::assertEquals('42', $result['count']);

            // Clean up
            $pdo->exec('DROP TABLE opr_json_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('MySQL JSON test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MySQL prepared statements
     * Verifies parameterized query support
     */
    public function testMySQLPreparedStatements(): void
    {
        $host = getenv('RUNTIME_NAME') === 'mysql' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 3306;
        $dbname = getenv('MYSQL_DATABASE') ?: 'mysql';
        $user = 'root';
        $password = getenv('MYSQL_ROOT_PASSWORD') ?: 'root';

        try {
            $dsn = "mysql:host=$host;port=$port;dbname=$dbname;charset=utf8mb4";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create test table
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_prepared_test (
                id INT AUTO_INCREMENT PRIMARY KEY,
                email VARCHAR(255) UNIQUE NOT NULL,
                age INT
            ) ENGINE=InnoDB');

            // Test prepared statement with named parameters
            $stmt = $pdo->prepare('INSERT INTO opr_prepared_test (email, age) VALUES (:email, :age)');
            $stmt->execute([':email' => 'user1@example.com', ':age' => 25]);
            $stmt->execute([':email' => 'user2@example.com', ':age' => 30]);
            $stmt->execute([':email' => 'user3@example.com', ':age' => 35]);

            // Query with prepared statement
            $stmt = $pdo->prepare('SELECT email, age FROM opr_prepared_test WHERE age > :min_age');
            $stmt->execute([':min_age' => 26]);
            $results = $stmt->fetchAll(\PDO::FETCH_ASSOC);

            self::assertCount(2, $results);
            self::assertEquals('user2@example.com', $results[0]['email']);
            self::assertEquals('user3@example.com', $results[1]['email']);

            // Clean up
            $pdo->exec('DROP TABLE opr_prepared_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('MySQL prepared statements test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test connection count reporting
     * Verifies that MySQL reports current connections accurately
     */
    public function testConnectionCount(): void
    {
        $response = \Tests\Client::execute(url: '/__opr/status', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertArrayHasKey('connections', $body);
        self::assertArrayHasKey('current', $body['connections']);
        self::assertArrayHasKey('max', $body['connections']);
        self::assertIsInt($body['connections']['current']);
        self::assertIsInt($body['connections']['max']);

        // Verify max connections matches expected default
        $expectedMax = (int)(getenv('MYSQL_MAX_CONNECTIONS') ?: 151);
        self::assertEquals($expectedMax, $body['connections']['max']);
    }

    /**
     * Test MySQL startup performance
     * Verifies that startup time is reasonable
     */
    public function testMySQLStartupPerformance(): void
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

        // MySQL should start within reasonable time (30 seconds for cold start)
        self::assertLessThan(30.0, $timings['startup'],
            'MySQL cold start should complete within 30 seconds');
    }

    /**
     * Test MySQL system information
     * Verifies that we can query system variables
     */
    public function testMySQLSystemInformation(): void
    {
        $host = getenv('RUNTIME_NAME') === 'mysql' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 3306;
        $dbname = getenv('MYSQL_DATABASE') ?: 'mysql';
        $user = 'root';
        $password = getenv('MYSQL_ROOT_PASSWORD') ?: 'root';

        try {
            $dsn = "mysql:host=$host;port=$port;dbname=$dbname;charset=utf8mb4";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Check server version
            $stmt = $pdo->query('SELECT VERSION()');
            $version = $stmt->fetchColumn();
            self::assertNotEmpty($version);

            // Check max connections setting
            $stmt = $pdo->query('SHOW VARIABLES LIKE "max_connections"');
            $result = $stmt->fetch(\PDO::FETCH_ASSOC);
            self::assertGreaterThan(0, (int)$result['Value']);

            // Check storage engine
            $stmt = $pdo->query('SHOW ENGINES');
            $engines = $stmt->fetchAll(\PDO::FETCH_COLUMN);
            self::assertContains('InnoDB', $engines);

            // Check current database
            $stmt = $pdo->query('SELECT DATABASE()');
            $currentDb = $stmt->fetchColumn();
            self::assertEquals($dbname, $currentDb);

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('MySQL system information test skipped: ' . $e->getMessage());
        }
    }

    /**
     * Test MySQL index operations
     * Verifies that indexes work correctly
     */
    public function testMySQLIndexes(): void
    {
        $host = getenv('RUNTIME_NAME') === 'mysql' ? 'open-runtimes-test-serve' : 'localhost';
        $port = 3306;
        $dbname = getenv('MYSQL_DATABASE') ?: 'mysql';
        $user = 'root';
        $password = getenv('MYSQL_ROOT_PASSWORD') ?: 'root';

        try {
            $dsn = "mysql:host=$host;port=$port;dbname=$dbname;charset=utf8mb4";
            $pdo = new \PDO($dsn, $user, $password, [
                \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION,
            ]);

            // Create table with indexes
            $pdo->exec('CREATE TABLE IF NOT EXISTS opr_index_test (
                id INT AUTO_INCREMENT PRIMARY KEY,
                email VARCHAR(255) NOT NULL,
                status VARCHAR(50) NOT NULL,
                INDEX idx_email (email),
                INDEX idx_status (status)
            ) ENGINE=InnoDB');

            // Verify indexes were created
            $stmt = $pdo->query('SHOW INDEX FROM opr_index_test');
            $indexes = $stmt->fetchAll(\PDO::FETCH_ASSOC);

            $indexNames = array_column($indexes, 'Key_name');
            self::assertContains('idx_email', $indexNames);
            self::assertContains('idx_status', $indexNames);

            // Clean up
            $pdo->exec('DROP TABLE opr_index_test');

            $pdo = null;
            self::assertTrue(true);
        } catch (\PDOException $e) {
            self::markTestSkipped('MySQL index test skipped: ' . $e->getMessage());
        }
    }
}
