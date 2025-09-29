package io.openruntimes.kotlin

import com.google.gson.GsonBuilder
import java.io.ByteArrayOutputStream
import java.io.FileWriter
import java.io.IOException
import java.io.PrintStream
import java.lang.System
import java.time.Instant
import kotlin.collections.List
import kotlin.collections.Map
import kotlin.collections.Set
import kotlin.random.Random

class RuntimeLogger(
    var status: String?,
    var id: String?,
) {
    var enabled = false
    var includesNativeInfo = false

    var streamLogs: FileWriter? = null
    var streamErrors: FileWriter? = null

    var customStdStream: ByteArrayOutputStream? = null

    var nativeLogsCache: PrintStream? = null
    var nativeErrorsCache: PrintStream? = null

    companion object {
        private val gson = GsonBuilder().serializeNulls().create()
        public val TYPE_ERROR = "error"
        public val TYPE_LOG = "log"
    }

    init {
        this.customStdStream = ByteArrayOutputStream()

        if (this.status == null) {
            this.status = ""
        }

        if (this.id == null) {
            this.id = ""
        }

        if (this.status.equals("enabled") || this.status.equals("")) {
            this.enabled = true
        } else {
            this.enabled = false
            this.id = ""
        }

        if (this.enabled) {
            var serverEnv = System.getenv("OPEN_RUNTIMES_ENV")
            if (serverEnv == null) {
                serverEnv = ""
            }

            if (this.id.equals("")) {
                if (serverEnv.equals("development")) {
                    this.id = "dev"
                } else {
                    this.id = this.generateId()
                }
            }

            this.streamLogs = FileWriter("/mnt/logs/" + this.id + "_logs.log", true)
            this.streamErrors = FileWriter("/mnt/logs/" + this.id + "_errors.log", true)
        }
    }

    fun write(
        messages: Array<out Any>,
        type: String = RuntimeLogger.TYPE_LOG,
        native: Boolean = false,
    ) {
        if (this.enabled == false) {
            return
        }

        if (native && !this.includesNativeInfo) {
            this.includesNativeInfo = true

            this.write(arrayOf("Native logs detected. Use context.log() or context.error() for better experience."), type, native)
        }

        var stream: FileWriter? = this.streamLogs

        if (type == RuntimeLogger.TYPE_ERROR) {
            stream = this.streamErrors
        }

        var stringLog: String = ""

        var i = 0
        for (message in messages) {
            if (message is Map<*, *> || message is List<*> || message is Set<*>) {
                stringLog += gson.toJson(message)
            } else {
                stringLog += message.toString()
            }

            if (i < messages.size - 1) {
                stringLog += " "
            }

            i += 1
        }

        if (stream != null) {
            if (stringLog.length > 8000) {
                stringLog = stringLog.substring(0, 8000)
                stringLog += "... Log truncated due to size limit (8000 characters)"
            }

            try {
                stream.write(stringLog)
            } catch (e: IOException) {
                // Silently fail to prevent 500 errors in runtime
                // Log write failures should not crash the runtime
            }
        }
    }

    fun end() {
        if (!this.enabled) {
            return
        }

        this.enabled = false

        this.streamLogs?.close()
        this.streamErrors?.close()
    }

    fun overrideNativeLogs() {
        this.nativeLogsCache = System.out
        this.nativeErrorsCache = System.err

        val customStd: PrintStream = PrintStream(this.customStdStream)
        System.setOut(customStd)
        System.setErr(customStd)
    }

    fun revertNativeLogs() {
        System.out.flush()
        System.err.flush()

        System.setOut(this.nativeLogsCache)
        System.setErr(this.nativeErrorsCache)

        if (!this.customStdStream.toString().isEmpty()) {
            this.write(arrayOf(customStdStream.toString()), RuntimeLogger.TYPE_LOG, true)
        }
    }

    fun generateId(padding: Int = 7): String {
        val now = Instant.now()
        val sec = now.epochSecond
        val usec = (System.nanoTime() / 1000) % 1000
        val baseId = "%08x%05x".format(sec, usec)
        val randomPadding =
            (1..padding)
                .map { Random.nextInt(0, 16).toString(16) }
                .joinToString("")

        return baseId + randomPadding
    }
}
