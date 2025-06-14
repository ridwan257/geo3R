
document.getElementById('home').style.display = "block";

create_column_dropdown('pdata-dropdown-container', 'column_dropdown', 'pdata-viewer');
create_column_dropdown('fdata-dropdown-container', 'column_dropdown2', 'fdata-viewer');

// ====================================================================================
// ====================================================================================

// adding event listener to buttons
// ====================================================================================
// ====================================================================================

// ====================================================================================
// ====================================================================================
document.getElementById('reload-plt-btn').addEventListener('click', async () => {
	await update_volcano_plot();
});
document.getElementById('reset-plt-btn').addEventListener('click', async () => {
	reset_volcano_options();
	await update_volcano_plot();
});




// document.getElementById('load-geo-btn').click();
// ====================================================================================
// initializing all
// ====================================================================================
reset_analysis_options();

// document.getElementById('geo-id').value = 'GSE179192'















