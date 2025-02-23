
<template>
    <p>OK Response</p>
</template>

<script setup>
    for (let i = 1; i <= 3; i++) {
        console.log("Concurrent Log " + i);
        await new Promise(resolve => setTimeout(resolve, 500 + Math.random() * 500));
    }
</script>
