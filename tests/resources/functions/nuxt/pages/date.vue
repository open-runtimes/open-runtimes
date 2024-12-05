
<template>
    <p id="date">{{ date }}</p>
</template>

<script setup>
    const date = new Date().toISOString();
</script>
