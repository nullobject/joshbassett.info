import { defineConfig } from 'astro/config';

// https://astro.build/config
export default defineConfig({
  // Enable Custom Markdown options, plugins, etc.
  markdown: {
    syntaxHighlight: 'shiki',
    shikiConfig: {
      themes: {
        light: 'rose-pine-dawn',
        dark: 'rose-pine-moon',
      },
      // Learn more about this configuration here:
      // https://docs.astro.build/en/guides/markdown-content/#syntax-highlighting
    },
  },
});
