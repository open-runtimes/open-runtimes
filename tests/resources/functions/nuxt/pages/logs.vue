
<template>
    <p>[DATE_START]{{ msg }}[DATE_END]</p>
</template>

<script setup>
    console.log("A log printed");
    console.error("An error printed");

    const msg = "All logs printed";
</script>
