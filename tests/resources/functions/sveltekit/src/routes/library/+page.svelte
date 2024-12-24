<script>
	/** @type {{ data: import('./$types').PageData }} */
	let { data } = $props();
</script>

<p>[UUID_START]{data.msg}[UUID_END]</p>