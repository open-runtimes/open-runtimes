
<template>
    <p>[UUID_START]{{ msg }}[UUID_END]</p>
</template>

<script setup>
    import { faker } from '@faker-js/faker';

    const id = faker.string.uuid();

    const msg = "My UUID is: " + id;
</script>
