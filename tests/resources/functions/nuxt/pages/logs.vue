
<template>
    <p id="date">{{ msg }}</p>
</template>

<script setup>
    console.log("A log printed");
    console.error("An error printed");

    const msg = "All logs printed";
</script>
