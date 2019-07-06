/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

// List of projects/orgs using your project for the users page.
const users = [
];

const baseUrl = '/irrec/' // Base URL for your project */

const siteConfig = {
  title: 'irrec', // Title for your website.
  tagline: 'composable regular expressions',
  url: 'https://github.com/ceedubs/irrec', // Your website URL
  baseUrl: baseUrl,
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'irrec',
  organizationName: 'ceedubs',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  customDocsPath: "irrec-docs/target/mdoc",

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    { doc: 'overview', label: 'Docs'},
    { href: `${baseUrl}api/ceedubs/irrec/regex/index.html`, label: 'API'},
    { href: 'https://github.com/ceedubs/irrec', label: 'GitHub' },
  ],

  /* path to images for header/footer */
  headerIcon: 'img/irrec-logo.png',
  footerIcon: 'img/irrec-logo.png',
  favicon: 'img/favicon.ico',

  /* Colors for website */
  colors: {
    primaryColor: '#17B890',
    secondaryColor: '#5E807F',
  },

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Cody Allen`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
  },

  // Add custom scripts here that would be placed in <script> tags.
  //scripts: ['https://buttons.github.io/buttons.js'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: 'img/undraw_online.svg',

  separateCss: ["api"],

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

};

module.exports = siteConfig;
