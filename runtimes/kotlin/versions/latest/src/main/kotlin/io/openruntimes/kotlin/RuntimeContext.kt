package io.openruntimes.kotlin

class RuntimeContext(
    val req: RuntimeRequest,
    val res: RuntimeResponse,
    val logger: RuntimeLogger,
) {
    fun log(vararg messages: Any) {
        this.logger.write(messages, RuntimeLogger.TYPE_LOG)
        this.logger.write(arrayOf("\n"), RuntimeLogger.TYPE_LOG)
    }

    fun error(vararg messages: Any) {
        this.logger.write(messages, RuntimeLogger.TYPE_ERROR)
        this.logger.write(arrayOf("\n"), RuntimeLogger.TYPE_ERROR)
    }
}
