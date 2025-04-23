
<template>
    <p>[DATE_START]{{ date }}[DATE_END]</p>
</template>

<script setup>
    const date = new Date().toISOString();
</script>
