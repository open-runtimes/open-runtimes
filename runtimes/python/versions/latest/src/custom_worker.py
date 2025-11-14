from aiohttp.worker import GunicornWebWorker
import asyncio


class CustomGunicornWebWorker(GunicornWebWorker):
    def init_process(self):
        # Ensure event loop exists before aiohttp tries to close it
        try:
            asyncio.get_event_loop()
        except RuntimeError:
            asyncio.set_event_loop(asyncio.new_event_loop())

        super().init_process()
