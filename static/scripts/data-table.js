document.addEventListener('DOMContentLoaded', function() {
  fetch('~/blog/daehl-e/python/excel365/M.csv')  // or '/scripts/data.csv'
    .then(response => response.json()) // or response.text() for CSV
    .then(data => {
      const table = document.getElementById('data-table');
      let html = '<table><thead><tr><th>Column1</th><th>Column2</th></tr></thead><tbody>';
      data.forEach(row => {
        html += `<tr><td>${row.column1}</td><td>${row.column2}</td></tr>`;
      });
      html += '</tbody></table>';
      table.innerHTML = html;
    });
});