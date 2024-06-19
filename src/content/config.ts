import { defineCollection, z } from 'astro:content';

const blog = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    author: z.string().default('Joshua Bassett'),
    date: z.coerce.date(),
  }),
});

export const collections = { blog };
