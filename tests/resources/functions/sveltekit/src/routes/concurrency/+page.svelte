<script>
	/** @type {{ data: import('./$types').PageData }} */
	let { data } = $props();
</script>

<p>{data.ok ? 'OK Response' : 'Failed'}</p>